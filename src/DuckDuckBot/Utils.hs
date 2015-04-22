{-# LANGUAGE RankNTypes #-}

module DuckDuckBot.Utils(
    isPrivMsgCommand,
    maybeGetPrivMsgReplyTarget,
    liftMaybe,
    sourceChan,
    sinkChan,
    takeIRCMessage,
    getCurrentMonotonicTime
) where

import DuckDuckBot.Types

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMChan

import Data.Int
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC

import qualified Data.ByteString as B

import qualified Network.IRC as IRC

import System.Clock

isPrivMsgCommand :: B.ByteString -> IRC.Message -> Bool
isPrivMsgCommand c (IRC.Message _ "PRIVMSG" [_, s]) = (commandString `B.append` " ") `B.isPrefixOf` s || commandString == s
                                                        where
                                                            commandString = "!" `B.append` c
isPrivMsgCommand _ _                                  = False

maybeGetPrivMsgReplyTarget :: IRC.Message -> Maybe B.ByteString
maybeGetPrivMsgReplyTarget (IRC.Message _ "PRIVMSG" [target, _])
                            | isChannel target = Just target
                            where
                                isChannel = ("#" `B.isPrefixOf`)
maybeGetPrivMsgReplyTarget (IRC.Message (Just (IRC.NickName nickname _ _)) _ _)
                            | nickname /= B.empty = Just nickname
maybeGetPrivMsgReplyTarget _                      = Nothing

liftMaybe :: (MonadPlus m) => Maybe a -> MaybeT m a
liftMaybe = maybe mzero return

sourceChan :: MonadIO m => TMChan a -> Producer m a
sourceChan chan = loop
    where
        loop = do
            a <- liftIO . atomically $ readTMChan chan
            case a of
                Just a' -> yieldOr a' (liftIO . atomically $ closeTMChan chan) >> loop
                Nothing -> return ()

sinkChan :: MonadIO m => TMChan a -> Consumer a m ()
sinkChan chan = CC.mapM_ (liftIO . atomically . writeTMChan chan)

takeIRCMessage :: Monad m => Conduit InMessage m IRC.Message
takeIRCMessage = CL.map unwrapInIRCMessage
    where
        unwrapInIRCMessage (InIRCMessage m) = m

-- New versions return Int64 so it works for more than 136 years,
-- which makes the fromIntegral unnecessary
--
-- Also we don't need sub-second precision here
getCurrentMonotonicTime :: IO Int64
getCurrentMonotonicTime = fmap (fromIntegral . sec) (getTime Monotonic)

