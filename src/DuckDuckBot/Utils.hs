module DuckDuckBot.Utils(
    isPrivMsgCommand,
    getPrivMsgReplyTarget,
    untilFalse
) where

import Control.Monad (when)

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import qualified Network.IRC as IRC

isPrivMsgCommand :: B.ByteString -> IRC.Message -> Bool
isPrivMsgCommand c (IRC.Message _ "PRIVMSG" (_:s:[])) = ((commandString `B.append` " ") `B.isPrefixOf` s || commandString == s)
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
