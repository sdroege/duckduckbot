module DuckDuckBot.Commands.Ddg (
  ddgCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils
import qualified DuckDuckBot.Commands.DdgQuery as DQ

import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.URI as URI
import qualified Network.IRC as IRC

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Trans.Maybe

ddgCommandHandlerMetadata :: MessageHandlerMetadata
ddgCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "ddg",
    messageHandlerMetadataCommands = ["!ddg"],
    messageHandlerMetadataHandler = ddgCommandHandler
}

ddgCommandHandler :: MessageHandler
ddgCommandHandler inChan outChan = liftIO $ HTTP.withManager HTTPS.tlsManagerSettings $ \manager ->
    sourceChan inChan
        =$= takeIRCMessage
        =$= CL.filter isDdgCommand
        =$= CL.mapM_ (handleDdgCommand manager outChan)
        $$ CL.sinkNull

    where
        isDdgCommand = isPrivMsgCommand "ddg"

handleDdgCommand :: MonadIO m => HTTP.Manager -> Chan OutMessage -> IRC.Message -> m ()
handleDdgCommand manager outChan m
        | (Just target) <- maybeGetPrivMsgReplyTarget m
        , (Just query)  <- parseQueryString m
        = liftIO $ void $ async (handleQuery outChan manager target query)
    where
        parseQueryString m' | (_:s:[]) <- IRC.msg_params m'
                            = case extractQuery s of
                                q | q == B.empty -> Nothing
                                  | otherwise    -> Just q
        parseQueryString _  = Nothing

        extractQuery = B.dropWhile isSpaceB . B.drop 4
        isSpaceB = (== fromIntegral (ord ' '))

handleDdgCommand _ _ _ = return ()

handleQuery :: Chan OutMessage -> HTTP.Manager -> B.ByteString -> B.ByteString -> IO ()
handleQuery outChan manager target query = void $ runMaybeT $ do
    let queryString = UB.toString query
    response <- liftMaybe <=< liftIO $ DQ.query manager queryString

    answer <- liftMaybe $ generateAnswer queryString response
    liftIO $ writeChan outChan (OutIRCMessage $ answerMessage (UB.fromString answer))

    where
        answerMessage answer = IRC.Message {  IRC.msg_prefix = Nothing,
                                              IRC.msg_command = "PRIVMSG",
                                              IRC.msg_params = [target, answer]
                                           }

generateAnswer :: String -> DQ.Results -> Maybe String
generateAnswer q r = if null answer then Nothing else Just answer
    where
        answer = take n a ++ dots ++ take m b
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

