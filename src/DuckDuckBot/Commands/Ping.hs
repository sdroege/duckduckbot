module DuckDuckBot.Commands.Ping (
  pingCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import qualified Network.IRC as IRC

import Data.Int
import Data.Conduit
import qualified Data.Conduit.List as CL

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async

import Control.Monad.IO.Class

import Control.Exception

pingCommandHandlerMetadata :: MessageHandlerMetadata
pingCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "ping",
    messageHandlerMetadataCommands = [],
    messageHandlerMetadataHandler = pingCommandHandler
}

pingCommandHandler :: MessageHandler
pingCommandHandler inChan outChan = do
    currentTime <- liftIO getCurrentMonotonicTime
    lastMessageTime <- liftIO $ newMVar currentTime
    serverName <- liftIO (newMVar Nothing :: IO (MVar (Maybe IRC.ServerName)))

    liftIO $ bracket (async $ checkTimeout serverName lastMessageTime outChan) cancel $ \a -> do
        link a

        sourceChan inChan
            =$= takeIRCMessage
            =$= CL.mapMaybeM (handlePingCommand serverName lastMessageTime)
            $$ CL.map OutIRCMessage
            =$= sinkChan outChan

handlePingCommand :: MVar (Maybe IRC.ServerName) -> MVar Int64 -> IRC.Message -> IO (Maybe IRC.Message)
handlePingCommand serverName lastMessageTime msg = do
    liftIO $ modifyMVar_ lastMessageTime (const getCurrentMonotonicTime)

    case msg of
        (IRC.Message (Just (IRC.Server name)) _ _) -> modifyMVar_ serverName (\_ -> return . Just $ name)
        _                                          -> return ()

    case msg of
        (IRC.Message _ "PING" targets) -> return $ Just (pongMessage targets)
        _                              -> return Nothing
    where
        pongMessage targets = IRC.Message { IRC.msg_prefix = Nothing,
                                            IRC.msg_command = "PONG",
                                            IRC.msg_params = targets
                                          }

checkTimeout :: MVar (Maybe IRC.ServerName) -> MVar Int64 -> Chan OutMessage -> IO ()
checkTimeout serverName lastMessageTime outChan = forever $ do
    threadDelay 30000000

    s <- readMVar serverName
    l <- readMVar lastMessageTime
    n <- getCurrentMonotonicTime

    let diff = n - l

    case s of
        Nothing              -> throwIO TimeoutException
        Just s' | diff > 300 -> throwIO TimeoutException
                | diff > 120 -> writeChan outChan $ OutIRCMessage (IRC.Message Nothing "PING" [s'])
                | otherwise  -> checkTimeout serverName lastMessageTime outChan

