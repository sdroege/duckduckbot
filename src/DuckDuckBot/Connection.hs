{-# LANGUAGE RankNTypes #-}

module DuckDuckBot.Connection (
    Connection,
    newConnection,
    closeConnection,
    sinkIRCConnection,
    sourceIRCConnection
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import Data.Foldable

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Attoparsec as CA
import qualified Network.Connection.Conduit as CC

import qualified Network.Connection as C
import qualified Network.IRC as IRC
import qualified Network.IRC.Parser as IRCP

import Control.Applicative
import Control.Monad.IO.Class

import System.IO

import Data.Attoparsec.ByteString

data Connection = Connection { getConnection :: C.Connection }

newConnection :: String -> Int -> Bool -> IO Connection
newConnection server port tls = do
    ctx <- C.initConnectionContext
    let tlsSettings = if tls then Just (C.TLSSettingsSimple False False True) else Nothing
        params = C.ConnectionParams {
                                    C.connectionHostname = server,
                                    C.connectionPort = fromIntegral port,
                                    C.connectionUseSecure = tlsSettings,
                                    C.connectionUseSocks = Nothing
                                  }

    c <- C.connectTo ctx params
    return (Connection c)

closeConnection :: Connection -> IO ()
closeConnection c = C.connectionClose (getConnection c)

sourceIRCConnection :: MonadIO m => Connection -> Source m IRC.Message
sourceIRCConnection c = CC.sourceConnection (getConnection c)
                        =$= CA.conduitParserEither messagecrlf
                        =$= CL.iterM (liftIO . B.putStr .
                                                 either (UB.fromString . ("ERROR: " ++) . (++ "\n") . show)
                                                        (("< " `B.append`) . (`B.append` "\n") . IRC.encode . snd))
                        =$= CL.mapMaybe (either (const Nothing) (Just . snd))

sinkIRCConnection :: MonadIO m => Connection -> Sink IRC.Message m ()
sinkIRCConnection c = CL.map IRC.encode
                      =$= sequenceSinks_ [ CL.map (`B.append` "\n\r")
                                           =$= CC.sinkConnection (getConnection c)
                                         , CL.map (("> " `B.append`) . (`B.append` "\n"))
                                           =$= CB.sinkHandle stdout]
    where
        sequenceSinks_ :: (Functor f, Foldable f, Monad m) => f (Sink i m r) -> Sink i m ()
        sequenceSinks_ = getZipSink . sequenceA_ . fmap ZipSink

messagecrlf :: Parser IRC.Message
messagecrlf = IRC.Message <$>
                     optionMaybe (IRCP.prefix <* IRCP.spaces)
                     <*> IRCP.command
                     <*> many (IRCP.spaces *> IRCP.parameter)
                     <* string "\r\n"
    where
        optionMaybe p = option Nothing (Just <$> p)
