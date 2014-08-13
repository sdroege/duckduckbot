module DuckDuckBot.Commands.Ddg (
  ddgCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import Data.Maybe
import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import Data.Aeson hiding (Result)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTPH
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.URI as URI
import qualified Network.IRC as IRC

import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Control.Concurrent
import Control.Exception

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
    response <- doQuery manager queryString
    when (isJust response) $ do
        let answer = generateAnswer queryString (fromJust response)
        when (answer /= "") $ send (answerMessage (UB.fromString answer))
    where
        answerMessage answer = IRC.Message {  IRC.msg_prefix = Nothing,
                                              IRC.msg_command = "PRIVMSG",
                                              IRC.msg_params = [target, answer]
                                           }

escapeQuery :: String -> String
escapeQuery = URI.escapeURIString URI.isUnescapedInURIComponent

generateAnswer :: String -> Results -> String
generateAnswer q r = take n a ++ dots ++ take m b
                        where
                            maxIRCLen = 400 -- arbitrary number below 512 to allow some space for channel, prefix, etc
                            (a, b) = generateAnswer' q r
                            m = min maxIRCLen (length b)
                            n = max 0 (maxIRCLen-m)
                            dots = if n < length a then "..." else ""

generateAnswer' :: String -> Results -> (String, String)
generateAnswer' q r | resultsAnswer r /= ""                          = (resultsAnswer r, " (http://ddg.gg/" ++ escapeQuery q ++ ")")
                    | resultsAbstractText r /= ""                    = (resultsAbstractText r, " (" ++ resultsAbstractSource r ++ ", http://ddg.gg/" ++ escapeQuery q ++ ")")
                    | resultsDefinition r /= ""                      = (resultsDefinition r, " (" ++ resultsDefinitionSource r ++ ", http://ddg.gg/" ++ escapeQuery q ++ ")")
                    | firstResultText (resultsRelatedTopics r) /= "" = (firstResultText (resultsRelatedTopics r), " (" ++ resultsAbstractSource r ++ ", http://ddg.gg/" ++ escapeQuery q ++ ")")
                    | resultsRedirect r /= ""                        = (resultsRedirect r, "")

                    | otherwise                                      = ("http://ddg.gg/" ++ escapeQuery q, "")
                 where
                     firstResultText ((Result { resultText=rt }):_) = rt
                     firstResultText (_:rs)                         = firstResultText rs
                     firstResultText _                              = ""

data Results = Results {
    resultsAbstract :: String,
    resultsAbstractText :: String,
    resultsAbstractSource :: String,
    resultsAbstractURL :: String,
    resultsImage :: String,
    resultsHeading :: String,
    resultsAnswer :: String,
    resultsRedirect :: String,
    resultsAnswerType :: String,
    resultsDefinition :: String,
    resultsDefinitionSource :: String,
    resultsDefinitionURL :: String,
    resultsRelatedTopics :: [Result],
    resultsResults :: [Result],
    resultsType :: String
} deriving (Show, Eq)

data Result = Result {
    resultResult :: String,
    resultIcon :: Maybe Icon,
    resultFirstURL :: String,
    resultText :: String
} | ResultGroup {
    resultGroupTopics :: [Result],
    resultGroupName :: String
} deriving (Show, Eq)

data Icon = Icon {
    iconURL :: String,
    iconHeight :: Int,
    iconWidth :: Int
} deriving (Show, Eq)

instance FromJSON Results where
    parseJSON (Object v) = Results <$>
                                v .: "Abstract" <*>
                                v .: "AbstractText" <*>
                                v .: "AbstractSource" <*>
                                v .: "AbstractURL" <*>
                                v .: "Image" <*>
                                v .: "Heading" <*>
                                v .: "Answer" <*>
                                v .: "Redirect" <*>
                                v .: "AnswerType" <*>
                                v .: "Definition" <*>
                                v .: "DefinitionSource" <*>
                                v .: "DefinitionURL" <*>
                                v .: "RelatedTopics" <*>
                                v .: "Results" <*>
                                v .: "Type"
    parseJSON _          = mzero

instance FromJSON Result where
    parseJSON (Object v) = Result <$>
                                v .: "Result" <*>
                                v .: "Icon" <*>
                                v .: "FirstURL" <*>
                                v .: "Text"
                            <|>
                           ResultGroup <$>
                                v .: "Topics" <*>
                                v .: "Name"
    parseJSON _          = mzero

instance FromJSON Icon where
    parseJSON (Object v) = Icon <$>
                                v .: "URL" <*>
                                -- Height and width can be an Int or ""
                                intOrString "Height" <*>
                                intOrString "Width"
                           where
                                intOrString s = v .: s <|> (stringTo0 <$> (v .: s))
                                stringTo0 :: String -> Int
                                stringTo0 _ = 0
    parseJSON _          = mzero

doQuery :: HTTP.Manager -> String -> IO (Maybe Results)
doQuery m s = do
    let q   = escapeQuery s
        url = "https://api.duckduckgo.com/?q=" ++ q ++ "&format=json&no_redirect=1&t=ddb&no_html=1"
    -- Catch all exceptions here and return nothing
    -- Better do nothing than crashing when we can't do the HTTP request
    handle (\(SomeException e) -> print ("Exception while handling Ddg request \"" ++ s ++ "\": " ++ show e) >> return Nothing) $ do
        baseReq <- HTTP.parseUrl url
        let headers = (HTTPH.hConnection, "Keep-Alive") : HTTP.requestHeaders baseReq
            req  = baseReq { HTTP.requestHeaders=headers }
        resp <- HTTP.httpLbs req m
        let d = decode (HTTP.responseBody resp) :: Maybe Results
        return d

