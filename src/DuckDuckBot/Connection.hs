module DuckDuckBot.Connection (
    Connection,
    newConnection,
    closeConnection,
    putMessageConnection,
    ConnectionReader,
    runConnectionReader,
    getMessage
) where

import Data.Maybe
import Data.Default
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UB

import System.IO
import System.X509 (getSystemCertificateStore)

import Network
import qualified Network.IRC as IRC
import qualified Network.IRC.Parser as IRCP
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSE
import qualified Crypto.Random.AESCtr as RNG

import Control.Applicative
import Control.Monad.State

import Data.Attoparsec.ByteString

data Connection = TCPConnection Handle | TLSConnection (TLS.Context, Handle)

newConnection :: String -> Int -> Bool -> IO Connection
newConnection server port True = do
    h <- newHandle server port
    caStore <- getSystemCertificateStore
    let params = (TLS.defaultParamsClient server B.empty) { TLS.clientSupported=def { TLS.supportedCiphers = TLSE.ciphersuite_medium }, TLS.clientShared=def { TLS.sharedCAStore = caStore } }
    rnd <- RNG.makeSystem
    ctx <- TLS.contextNew h params rnd
    TLS.handshake ctx
    return (TLSConnection (ctx, h))
newConnection server port False = TCPConnection `fmap` newHandle server port

newHandle :: String -> Int -> IO Handle
newHandle server port = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    hSetEncoding h utf8
    return h

closeConnection :: Connection -> IO ()
closeConnection (TCPConnection h) = hClose h
closeConnection (TLSConnection (ctx, h)) = TLS.bye ctx >> hClose h

putMessageConnection :: Connection -> IRC.Message -> IO ()
putMessageConnection (TCPConnection h)        m = do
    let s = IRC.encode m
    BL.hPutStr h (BL.fromChunks [s, "\r\n"])
    putStrLn ("> " ++ UB.toString s)

putMessageConnection (TLSConnection (ctx, _)) m = do
    let s = IRC.encode m
    TLS.sendData ctx (BL.fromChunks [s, "\r\n"])
    putStrLn ("> " ++ UB.toString s)

data ConnectionReadContext = ConnectionReadContext {
    ctxConnection :: Connection,
    ctxState :: Maybe (Result IRC.Message)
}

type ConnectionReader = StateT ConnectionReadContext
runConnectionReader :: (MonadIO m) => Connection -> ConnectionReader m a -> m a
runConnectionReader c f = evalStateT f ConnectionReadContext {ctxConnection = c, ctxState = Nothing}

-- TODO: Catch errors and return Nothing
getMessage :: (MonadIO m) => ConnectionReader m (Maybe IRC.Message)
getMessage = do
    c  <- gets ctxConnection
    case c of
        (TCPConnection h)        -> getMessageInternalTCP h
        (TLSConnection (ctx, _)) -> getMessageInternalTLS ctx

getMessageInternalTCP :: (MonadIO m) => Handle -> ConnectionReader m (Maybe IRC.Message)
getMessageInternalTCP h = do
    line <- liftIO $ B.hGetLine h
    liftIO $ putStrLn ("< " ++ UB.toString line)
    return (IRC.decode line)

getMessageInternalTLS :: (MonadIO m) => TLS.Context -> ConnectionReader m (Maybe IRC.Message)
getMessageInternalTLS ctx = do
    -- Read data or get leftover data from last run
    s    <- gets ctxState
    line <- case s of
                Just (Done t _) -> return t
                _               -> liftIO $ TLS.recvData ctx

    -- Here we either have a partial s or need to
    -- start a new parser (error, done, nothing)
    let r = case s of
                Just (Partial _) -> feed (fromJust s) line
                _                -> parse messagecrlf line

    -- Remember state
    c <- get
    put (c { ctxState = Just r})

    -- If we finished a parse, return the message, otherwise
    -- recurse to get more data
    case r of
        (Done _ msg) -> liftIO $ putStrLn ("< " ++ UB.toString (IRC.encode msg)) >> return (Just msg)
        _            -> getMessageInternalTLS ctx

messagecrlf :: Parser IRC.Message
messagecrlf = liftA3 IRC.Message
                     (optionMaybe (tokenize IRCP.prefix))
                     IRCP.command
                     (many (IRCP.spaces >> IRCP.parameter)) <* string "\r\n"
    where
        optionMaybe p = option Nothing (Just <$> p)
        tokenize p = p >>= \x -> IRCP.spaces >> return x
