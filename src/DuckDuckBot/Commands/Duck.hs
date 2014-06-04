module DuckDuckBot.Commands.Duck (
  duckCommandHandler
) where

import DuckDuckBot.Types

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
        isDuckCommand msg = command == "PRIVMSG" && length params == 2 && "!duck" `B.isPrefixOf` s
                            where
                                command = IRC.msg_command msg
                                params = IRC.msg_params msg
                                s = head (tail params)

        handleDuck msg = when (target /= B.empty) $ sendDuck target
                         where
                             target = getPrivMsgReplyTarget msg

        -- TODO: This should be in some utils module
        getPrivMsgReplyTarget msg | isChannel target  = target
                                  | isNickName prefix = getNickName prefix
                                  | otherwise         = B.empty
                                  where
                                      target = head (IRC.msg_params msg)
                                      prefix = IRC.msg_prefix msg
                                      isChannel = (UB.fromString "#" `B.isPrefixOf`)
                                      isNickName p = case p of
                                                         (Just (IRC.NickName {})) -> True
                                                         _                        -> False
                                      getNickName p = case p of
                                                         (Just (IRC.NickName n _ _)) -> n
                                                         _                           -> B.empty

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
rightBody  = ["\\\\_", "\\\\__", "~_", "~__"]
heads      = ["o", "O", "0", "@", "©", "®", "ð", "*", "ò", "ô", "ó", "ø", "⊕", "Ω", "ꙫ", "ꙩ", "Ꙩ", "ȯ", "◔", "õ", "ȯ", "⁰", "Ö", "Ó", "Ò", "Õ", "Ô", "ö"]

ducks :: [String]
ducks = [be ++ h ++ bo| be <- leftBeaks, bo <- leftBody, h <- heads]
        ++
        [bo ++ h ++ be| be <- rightBeaks, bo <- rightBody, h <- heads]

duck :: IO String
duck = do
          idx <- randomRIO (0, pred $ length ducks)
          return $ ducks !! idx
