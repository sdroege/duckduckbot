module DuckDuckBot.Commands.Ping (
  pingCommandHandler
) where

import DuckDuckBot.Types

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.Chan
import qualified Network.IRC as IRC

pingCommandHandler :: MessageHandler
pingCommandHandler cIn cOut = forever $ do
    msg <- liftIO $ readChan cIn
    case msg of
        InIRCMessage m | isPingCommand m -> handlePing m
        _                                -> return ()
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
