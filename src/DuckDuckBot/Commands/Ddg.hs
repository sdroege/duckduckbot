module DuckDuckBot.Commands.Ddg (
  ddgCommandHandler
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import Data.Maybe
import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import Data.Aeson hiding (Result)
import Data.Text hiding (head, tail, drop)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.URI as URI
import qualified Network.IRC as IRC

import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Control.Concurrent

ddgCommandHandler :: MessageHandler
ddgCommandHandler cIn cOut = forever $ do
    msg <- liftIO $ readChan cIn
    manager <- liftIO $ HTTP.newManager HTTPS.tlsManagerSettings
    case msg of
        InIRCMessage m | isDdgCommand m  -> handleDdg m manager
        Quit                             -> return () -- TODO: Handle quit
        _                                -> return ()
    where
        isDdgCommand = isPrivMsgCommand "ddg"

        handleDdg msg manager = when (target /= B.empty && query /= B.empty) $ liftIO $ void $ forkIO (handleQuery manager target query)
                                where
                                    target = getPrivMsgReplyTarget msg
                                    params = IRC.msg_params msg
                                    s = head (tail params)
                                    isSpace = (== fromIntegral (ord ' '))
                                    extractQuery = B.takeWhile (not . isSpace) . B.dropWhile isSpace . B.drop 4
                                    query = extractQuery s

        handleQuery manager target query = do
                                            response <- doQuery manager (UB.toString query)
                                            let answer = response
                                            when (isJust answer) $ writeChan cOut (answerMessage target (UB.fromString (show (fromJust answer))))

        answerMessage target answer = OutIRCMessage IRC.Message { IRC.msg_prefix = Nothing,
                                                                  IRC.msg_command = "PRIVMSG",
                                                                  IRC.msg_params = [target, answer]
                                                                }


data Results = Results {
    resultsAbstract :: Text,
    resultsAbstractText :: Text,
    resultsAbstractSource :: Text,
    resultsAbstractURL :: Text,
    resultsImage :: Text,
    resultsHeading :: Text,
    resultsAnswer :: Text,
    resultsRedirect :: Text,
    resultsAnswerType :: Text,
    resultsDefinition :: Text,
    resultsDefinitionSource :: Text,
    resultsDefinitionURL :: Text,
    resultsRelatedTopics :: [Result],
    resultsResults :: [Result],
    resultsType :: Text
} deriving (Show, Eq)

data Result = Result {
    resultResult :: Text,
    resultIcon :: Maybe Icon,
    resultFirstURL :: Text,
    resultText :: Text
} | ResultGroup {
    resultGroupTopics :: [Result],
    resultGroupName :: Text
} deriving (Show, Eq)

data Icon = Icon {
    iconURL :: Text,
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
                                intOrText "Height" <*>
                                intOrText "Width"
                           where
                                intOrText s = v .: s <|> (textTo0 <$> (v .: s))
                                textTo0 :: Text -> Int
                                textTo0 _ = 0
    parseJSON _          = mzero

doQuery :: HTTP.Manager -> String -> IO (Maybe Results)
doQuery m s = do
    let q   = URI.escapeURIString URI.isUnescapedInURIComponent s
        url = "https://api.duckduckgo.com/?q=" ++ q ++ "&format=json&no_redirect=1&t=ddb&no_html=1"
    req <- HTTP.parseUrl url

    resp <- HTTP.httpLbs req m

    let d = decode (HTTP.responseBody resp) :: Maybe Results
    return d

