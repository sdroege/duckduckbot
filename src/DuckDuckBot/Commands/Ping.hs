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
        InIRCMessage (IRC.Message _ "PING" targets) -> handlePing targets >> return True
        Quit                                        -> return False
        _                                           -> return True
    where
        handlePing targets = liftIO $ writeChan cOut (createPong targets)

        createPong targets = OutIRCMessage IRC.Message { IRC.msg_prefix = Nothing,
                                                         IRC.msg_command = "PONG",
                                                         IRC.msg_params = targets
                                                       }
