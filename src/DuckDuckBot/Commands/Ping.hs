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
        InIRCMessage m@(IRC.Message { IRC.msg_command = "PING" }) -> handlePing m
        Quit                                                      -> return () -- TODO: Handle quit
        _                                                         -> return ()
    where
        handlePing   m = liftIO $ writeChan cOut (pongFromPing m)
        pongFromPing m = OutIRCMessage IRC.Message { IRC.msg_prefix = Nothing,
                                                     IRC.msg_command = "PONG",
                                                     IRC.msg_params = IRC.msg_params m
                                                   }
