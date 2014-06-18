module DuckDuckBot.Commands.Duck (
  duckCommandHandler
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.Chan
import qualified Network.IRC as IRC

import System.Random

duckCommandHandler :: MessageHandler
duckCommandHandler cIn cOut = forever $ do
    msg <- liftIO $ readChan cIn
    case msg of
        InIRCMessage m | isDuckCommand m -> handleDuck m
        Quit                             -> return () -- TODO: Handle quit
        _                                -> return ()
    where
        isDuckCommand = isPrivMsgCommand "duck"

        handleDuck msg = when (target /= B.empty) $ sendDuck target
                         where
                             target = getPrivMsgReplyTarget msg

        sendDuck target = do
                             d <- liftIO duck
                             liftIO $ writeChan cOut (duckMessage target (UB.fromString d))

        duckMessage target d = OutIRCMessage IRC.Message { IRC.msg_prefix = Nothing,
                                                           IRC.msg_command = "PRIVMSG",
                                                           IRC.msg_params = [target, d]
                                                         }

--
-- The actual duck string creation
--
leftBeaks, rightBeaks :: [String]
leftBody, rightBody :: [String]
heads :: [String]
leftBeaks  = [">", "="]
rightBeaks = ["<", "="]
leftBody   = ["_/", "__/", "_~", "__~"]
rightBody  = ["\\_", "\\__", "~_", "~__"]
heads      = ["o", "O", "0", "@", "©", "®", "ð", "*", "ò", "ô", "ó", "ø", "⊕", "Ω", "ꙫ", "ꙩ", "Ꙩ", "ȯ", "◔", "õ", "ȯ", "⁰", "Ö", "Ó", "Ò", "Õ", "Ô", "ö"]

ducks :: [String]
ducks = [be ++ h ++ bo| be <- leftBeaks, bo <- leftBody, h <- heads]
        ++
        [bo ++ h ++ be| be <- rightBeaks, bo <- rightBody, h <- heads]

duck :: IO String
duck = do
          idx <- randomRIO (0, pred $ length ducks)
          return $ ducks !! idx
