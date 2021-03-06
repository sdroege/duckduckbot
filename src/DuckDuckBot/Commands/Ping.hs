{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module DuckDuckBot.Commands.Ping (
  pingCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import qualified Network.IRC as IRC

import Data.Conduit
import qualified Data.Conduit.List as CL

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

import Control.Concurrent.STM
import Control.Concurrent.STM.TMChan

import Control.Exception

pingCommandHandlerMetadata :: MessageHandlerMetadata
pingCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "ping",
    messageHandlerMetadataCommands = [],
    messageHandlerMetadataHandler = pingCommandHandler
}

sourceChanWithTimeout :: MonadIO m => TMChan a -> Int -> Producer m (Either Int a)
sourceChanWithTimeout chan tv = loop 0
    where
        timeout t = do
            v <- readTVar t
            unless v retry

        close = liftIO . atomically $ closeTMChan chan

        loop c = do
            t <- liftIO $ registerDelay tv
            a <- liftIO . atomically $
                (Right <$> readTMChan chan) `orElse` (timeout t *> return (Left (c + 1)))

            case a of
                Right (Just a') -> yieldOr (Right a') close >> loop 0
                Left c'         -> yieldOr (Left c')  close >> loop c'
                Right Nothing   -> return ()

pingCommandHandler :: MessageHandler
pingCommandHandler inChan outChan = flip evalStateT Nothing $
    sourceChanWithTimeout inChan 120000000
        =$= CL.mapMaybeM handlePingCommand
        $$ CL.map OutIRCMessage
        =$= sinkChan outChan

handlePingCommand :: (MonadIO m, MonadState (Maybe IRC.ServerName) m) => Either Int InMessage -> m (Maybe IRC.Message)
handlePingCommand msg = do
    case msg of
        Right (InIRCMessage (IRC.Message (Just (IRC.Server name)) _ _)) -> put (Just name)
        _                                                               -> return ()

    case msg of
        Right (InIRCMessage (IRC.Message _ "PING" targets)) -> return $ Just (pongMessage targets)
        Right _                                             -> return Nothing
        Left c | c < 4                                      -> do
                                                                    s <- get
                                                                    return $ pingMessage <$> s
               | otherwise                                  -> liftIO $ throwIO TimeoutException
    where
        pongMessage targets = IRC.Message { IRC.msg_prefix = Nothing,
                                            IRC.msg_command = "PONG",
                                            IRC.msg_params = targets
                                          }
        pingMessage target  = IRC.Message { IRC.msg_prefix = Nothing,
                                            IRC.msg_command = "PING",
                                            IRC.msg_params = [target]
                                          }

