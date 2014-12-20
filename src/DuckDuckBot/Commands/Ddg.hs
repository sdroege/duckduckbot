module DuckDuckBot.Commands.Ddg (
  ddgCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils
import qualified DuckDuckBot.Commands.DdgQuery as DQ

import Data.Maybe
import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.URI as URI
import qualified Network.IRC as IRC

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent

ddgCommandHandlerMetadata :: MessageHandlerMetadata
ddgCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "ddg",
    messageHandlerMetadataCommands = ["!ddg"],
    messageHandlerMetadataHandler = ddgCommandHandler
}

ddgCommandHandler :: MessageHandler
ddgCommandHandler = messageHandlerLoop run handleMessage

type DdgReader = ReaderT HTTP.Manager

run :: DdgReader (MessageHandlerEnvReader IO) () -> MessageHandlerEnvReader IO ()
run l = do
    manager <- liftIO $ HTTP.newManager HTTPS.tlsManagerSettings
    void $ runReaderT l manager
    liftIO $ HTTP.closeManager manager

handleMessage :: IRC.Message -> MessageHandlerSendMessage -> DdgReader (MessageHandlerEnvReader IO) ()
handleMessage msg send = do
    manager <- ask
    case msg of
        (IRC.Message _ _ (_:s:[])) | isDdgCommand msg -> when (target /= B.empty && query /= B.empty) $
                                                            liftIO $ void $ forkIO (handleQuery send manager target query)
                                                                where
                                                                    target = getPrivMsgReplyTarget msg
                                                                    query = extractQuery s
                                                                    extractQuery = B.dropWhile isSpaceB . B.drop 4
                                                                    isSpaceB = (== fromIntegral (ord ' '))
        _                                             -> return ()
    where
        isDdgCommand = isPrivMsgCommand "ddg"

handleQuery :: MessageHandlerSendMessage -> HTTP.Manager -> B.ByteString -> B.ByteString -> IO ()
handleQuery send manager target query = do
    let queryString = UB.toString query
    response <- DQ.query manager queryString
    when (isJust response) $ do
        let answer = generateAnswer queryString (fromJust response)
        when (answer /= "") $ send (answerMessage (UB.fromString answer))
    where
        answerMessage answer = IRC.Message {  IRC.msg_prefix = Nothing,
                                              IRC.msg_command = "PRIVMSG",
                                              IRC.msg_params = [target, answer]
                                           }

generateAnswer :: String -> DQ.Results -> String
generateAnswer q r = take n a ++ dots ++ take m b
                        where
                            maxIRCLen = 400 -- arbitrary number below 512 to allow some space for channel, prefix, etc
                            (a, b) = generateAnswer' q r
                            m = min maxIRCLen (length b)
                            n = max 0 (maxIRCLen-m)
                            dots = if n < length a then "..." else ""

generateAnswer' :: String -> DQ.Results -> (String, String)
generateAnswer' q r | DQ.resultsAnswer r /= ""                          = (DQ.resultsAnswer r, " (http://ddg.gg/" ++ escapedQuery ++ ")")
                    | DQ.resultsAbstractText r /= ""                    = (DQ.resultsAbstractText r, " (" ++ DQ.resultsAbstractSource r ++ ", http://ddg.gg/" ++ escapedQuery ++ ")")
                    | DQ.resultsDefinition r /= ""                      = (DQ.resultsDefinition r, " (" ++ DQ.resultsDefinitionSource r ++ ", http://ddg.gg/" ++ escapedQuery ++ ")")
                    | firstResultText (DQ.resultsRelatedTopics r) /= "" = (firstResultText (DQ.resultsRelatedTopics r), " (" ++ DQ.resultsAbstractSource r ++ ", http://ddg.gg/" ++ escapedQuery ++ ")")
                    | DQ.resultsRedirect r /= ""                        = (DQ.resultsRedirect r, "")

                    | otherwise                                         = ("http://ddg.gg/" ++ escapedQuery, "")
                 where
                     firstResultText ((DQ.Result { DQ.resultText=rt }):_) = rt
                     firstResultText (_:rs)                               = firstResultText rs
                     firstResultText _                                    = ""
                     escapedQuery                                         = URI.escapeURIString URI.isUnescapedInURIComponent q

