module DuckDuckBot.Commands.Ping (
  pingCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import Control.Monad.Reader
import qualified Network.IRC as IRC

pingCommandHandlerMetadata :: MessageHandlerMetadata
pingCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "ping",
    messageHandlerMetadataCommands = [],
    messageHandlerMetadataHandler = pingCommandHandler
}

pingCommandHandler :: MessageHandler
pingCommandHandler = messageHandlerLoop id handleMessage

handleMessage :: IRC.Message -> MessageHandlerSendMessage -> MessageHandlerEnvReader IO ()
handleMessage msg send =
    case msg of
        (IRC.Message _ "PING" targets) -> liftIO $ send (createPong targets)
        _                              -> return ()
    where
        createPong targets = IRC.Message { IRC.msg_prefix = Nothing,
                                           IRC.msg_command = "PONG",
                                           IRC.msg_params = targets
                                         }
