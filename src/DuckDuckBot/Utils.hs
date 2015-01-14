{-# LANGUAGE RankNTypes #-}

module DuckDuckBot.Utils(
    isPrivMsgCommand,
    maybeGetPrivMsgReplyTarget,
    liftMaybe,
    sourceChan,
    sinkChan,
    takeIRCMessage
) where

import DuckDuckBot.Types

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Concurrent (Chan, readChan, writeChan)

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC

import qualified Data.ByteString as B

import qualified Network.IRC as IRC

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

sourceChan :: MonadIO m => Chan a -> Producer m a
sourceChan chan = CC.repeatM (liftIO $ readChan chan)

sinkChan :: MonadIO m => Chan a -> Consumer a m ()
sinkChan chan = CC.mapM_ (liftIO . writeChan chan)

takeIRCMessage :: Monad m => Conduit InMessage m IRC.Message
takeIRCMessage = CC.takeWhile (not . isQuit) =$= CL.mapMaybe unwrapInIRCMessage
    where
        isQuit Quit = True
        isQuit _    = False

        unwrapInIRCMessage (InIRCMessage m) = Just m
        unwrapInIRCMessage _                = Nothing

