module DuckDuckBot.Commands.Duck (
  duckCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Data.ByteString.UTF8 as UB

import Control.Monad.Reader
import qualified Network.IRC as IRC

import System.Random

duckCommandHandlerMetadata :: MessageHandlerMetadata
duckCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "duck",
    messageHandlerMetadataCommands = ["!duck"],
    messageHandlerMetadataHandler = duckCommandHandler
}

duckCommandHandler :: MessageHandler
duckCommandHandler inChan outChan =
    sourceChan inChan
        =$= takeIRCMessage
        =$= CL.filter isDuckCommand
        =$= CL.mapMaybeM handleDuckCommand
        $$ CL.map OutIRCMessage
        =$= sinkChan outChan

    where
        isDuckCommand = isPrivMsgCommand "duck"

        handleDuckCommand m | (Just target) <- maybeGetPrivMsgReplyTarget m
                              = do
                                  d <- liftIO duck
                                  return $ Just (duckMessage target (UB.fromString d))
        handleDuckCommand _   = return Nothing

        duckMessage target d = IRC.Message { IRC.msg_prefix = Nothing,
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
