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
isPrivMsgCommand c msg =    command == "PRIVMSG" && length params == 2 && (("!" `B.append` c `B.append` " ") `B.isPrefixOf` s || ("!" `B.append` c) == s)
                            where
                                command = IRC.msg_command msg
                                params = IRC.msg_params msg
                                s = head (tail params)

getPrivMsgReplyTarget :: IRC.Message -> B.ByteString
getPrivMsgReplyTarget msg   | isChannel target  = target
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

untilFalse :: (Monad m) => m Bool -> m ()
untilFalse p = do
    x <- p
    when x $ untilFalse p
