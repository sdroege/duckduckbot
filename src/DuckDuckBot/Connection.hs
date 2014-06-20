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
import Control.Monad.Reader
import Control.Exception

import Data.Attoparsec.ByteString hiding (try)

data Connection = TCPConnection Handle | TLSConnection (TLS.Context, Handle)

newConnection :: String -> Int -> Bool -> IO Connection
newConnection server port True = bracketOnError (newHandle server port) hClose $ \h -> do
    caStore <- getSystemCertificateStore
    let params = (TLS.defaultParamsClient server B.empty) {
                                                            TLS.clientSupported=def {
                                                                                        TLS.supportedCiphers = TLSE.ciphersuite_medium
                                                                                    },
                                                            TLS.clientShared=def    {
                                                                                        TLS.sharedCAStore = caStore
                                                                                    }
                                                            }
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
putMessageConnection (TCPConnection h) m = do
    -- Catch all possible exceptions when evaluating encoding
    -- as a IRC message and make sure we have a 
    evS <- liftIO (try (evaluate (IRC.encode m)) :: IO (Either SomeException UB.ByteString))
    case evS of
        Right s -> do
                        BL.hPutStr h (BL.fromChunks [s, "\r\n"])
                        putStrLn ("> " ++ UB.toString s)
        Left e  -> liftIO $ print ("Exception in putMessageConnection: " ++ show e)

putMessageConnection (TLSConnection (ctx, _)) m = do
    let s = IRC.encode m
    TLS.sendData ctx (BL.fromChunks [s, "\r\n"])
    putStrLn ("> " ++ UB.toString s)

type ConnectionReadState = Maybe (Result IRC.Message)

type ConnectionReader m = ReaderT Connection (StateT ConnectionReadState m)
runConnectionReader :: (MonadIO m) => Connection -> ConnectionReader m a -> m a
runConnectionReader c f = evalStateT (runReaderT f c) Nothing

getMessage :: (MonadIO m) => ConnectionReader m IRC.Message
getMessage = do
    c  <- ask
    case c of
        (TCPConnection h)        -> getMessageInternalTCP h
        (TLSConnection (ctx, _)) -> getMessageInternalTLS ctx

getMessageInternalTCP :: (MonadIO m) => Handle -> ConnectionReader m IRC.Message
getMessageInternalTCP h = do
    line <- liftIO $ B.hGetLine h
    liftIO $ putStrLn ("< " ++ UB.toString line)
    let msg = IRC.decode line
    case msg of
        Just m  -> return m
        Nothing -> getMessageInternalTCP h

getMessageInternalTLS :: (MonadIO m) => TLS.Context -> ConnectionReader m IRC.Message
getMessageInternalTLS ctx = do
    -- Read data or get leftover data from last run
    s    <- get
    line <- case s of
                Just (Done t _) -> return t
                _               -> liftIO $ TLS.recvData ctx

    -- Here we either have a partial s or need to
    -- start a new parser (error, done, nothing)
    let r = case s of
                Just (Partial _) -> feed (fromJust s) line
                _                -> parse messagecrlf line

    -- Remember state
    put (Just r)

    -- If we finished a parse, return the message, otherwise
    -- recurse to get more data
    case r of
        (Done _ msg) -> liftIO $ putStrLn ("< " ++ UB.toString (IRC.encode msg)) >> return msg
        _            -> getMessageInternalTLS ctx

messagecrlf :: Parser IRC.Message
messagecrlf = liftA3 IRC.Message
                     (optionMaybe (tokenize IRCP.prefix))
                     IRCP.command
                     (many (IRCP.spaces >> IRCP.parameter)) <* string "\r\n"
    where
        optionMaybe p = option Nothing (Just <$> p)
        tokenize p = p >>= \x -> IRCP.spaces >> return x
