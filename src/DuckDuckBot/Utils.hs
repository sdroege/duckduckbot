module DuckDuckBot.Utils(
    isPrivMsgCommand,
    getPrivMsgReplyTarget,
    untilFalse,
    messageHandlerLoop
) where

import DuckDuckBot.Types

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import qualified Network.IRC as IRC

isPrivMsgCommand :: B.ByteString -> IRC.Message -> Bool
isPrivMsgCommand c (IRC.Message _ "PRIVMSG" (_:s:[])) = (commandString `B.append` " ") `B.isPrefixOf` s || commandString == s
                                                        where
                                                            commandString = "!" `B.append` c
isPrivMsgCommand _ _                                  = False

getPrivMsgReplyTarget :: IRC.Message -> B.ByteString
getPrivMsgReplyTarget (IRC.Message _ "PRIVMSG" (target:_:[])) | isChannel target  = target
                                                                                    where
                                                                                        isChannel = (UB.fromString "#" `B.isPrefixOf`)
getPrivMsgReplyTarget (IRC.Message (Just (IRC.NickName nickname _ _)) _ _)        = nickname
getPrivMsgReplyTarget _                                                           = B.empty

untilFalse :: (Monad m) => m Bool -> m ()
untilFalse p = do
    x <- p
    when x $ untilFalse p

messageHandlerLoop :: (MonadIO m) => (m () -> MessageHandlerEnvReader IO ()) -> (IRC.Message -> MessageHandlerSendMessage -> m ()) -> Chan InMessage -> Chan OutMessage -> MessageHandlerEnvReader IO ()
messageHandlerLoop run handleMessage cIn cOut =
    run $ do
        let send m = writeChan cOut (OutIRCMessage m)
        untilFalse $ do
            cMsg <- liftIO $ readChan cIn
            case cMsg of
                InIRCMessage msg -> handleMessage msg send >> return True
                Quit             -> return False

