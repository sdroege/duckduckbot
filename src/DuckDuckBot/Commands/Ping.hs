module DuckDuckBot.Commands.Ping (
  pingCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import Control.Monad.Reader
import Control.Concurrent.Chan
import qualified Network.IRC as IRC

pingCommandHandlerMetadata :: MessageHandlerMetadata
pingCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "ping",
    messageHandlerMetadataCommands = [],
    messageHandlerMetadataHandler = pingCommandHandler
}

pingCommandHandler :: MessageHandler
pingCommandHandler cIn cOut = untilFalse $ do
    msg <- liftIO $ readChan cIn
    case msg of
        InIRCMessage m | isPingCommand m -> handlePing m >> return True
        Quit                             -> return False
        _                                -> return True
    where
        isPingCommand msg = command == "PING" && length params <= 2
                            where
                                command = IRC.msg_command msg
                                params = IRC.msg_params msg

        handlePing   m = liftIO $ writeChan cOut (pongFromPing m)

        pongFromPing m = OutIRCMessage IRC.Message { IRC.msg_prefix = Nothing,
                                                     IRC.msg_command = "PONG",
                                                     IRC.msg_params = IRC.msg_params m
                                                   }
