module DuckDuckBot.Commands.Ping (
  pingCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import qualified Network.IRC as IRC

import Data.Conduit
import qualified Data.Conduit.List as CL

pingCommandHandlerMetadata :: MessageHandlerMetadata
pingCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "ping",
    messageHandlerMetadataCommands = [],
    messageHandlerMetadataHandler = pingCommandHandler
}

pingCommandHandler :: MessageHandler
pingCommandHandler inChan outChan =
    sourceChan inChan
        =$= takeIRCMessage
        =$= CL.mapMaybe handlePingCommand
        $$ CL.map OutIRCMessage
        =$= sinkChan outChan

handlePingCommand :: IRC.Message -> Maybe IRC.Message
handlePingCommand msg =
    case msg of
        (IRC.Message _ "PING" targets) -> Just (pongMessage targets)
        _                              -> Nothing
    where
        pongMessage targets = IRC.Message { IRC.msg_prefix = Nothing,
                                            IRC.msg_command = "PONG",
                                            IRC.msg_params = targets
                                          }
