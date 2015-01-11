module DuckDuckBot.Commands.Translate (
  translateCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils
import DuckDuckBot.Compat

import System.Environment

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as UB

import Data.Time.Clock
import Data.Maybe

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

import qualified Text.XML.Light as XML

import Data.Aeson

import Safe

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Network.IRC as IRC

import Control.Concurrent
import Control.Concurrent.Async

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Exception hiding (try)

import Text.Parsec hiding ((<|>))
import Text.Parsec.String

data OAuth = OAuth
    { oAuthTokenType   :: String
    , oAuthAccessToken :: String
    , oAuthExpiresIn   :: Maybe Int
    , oAuthScope       :: String
    } deriving (Show, Eq)

instance FromJSON OAuth where
    parseJSON (Object v) = OAuth
                            <$> v .: "token_type"
                            <*> v .: "access_token"
                            <*> (readMay <$> (v .: "expires_in"))
                            <*> v .: "scope"
    parseJSON _          = mzero

translateCommandHandlerMetadata :: MessageHandlerMetadata
translateCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "translate",
    messageHandlerMetadataCommands = ["!x"],
    messageHandlerMetadataHandler = translateCommandHandler
}

translateCommandHandler :: MessageHandler
translateCommandHandler inChan outChan = liftIO $ HTTP.withManager HTTPS.tlsManagerSettings $ \manager -> do
    oauth <- newMVar Nothing
    clientId <- liftM (fromMaybe "") $ lookupEnv "DDB_TRANSLATE_CLIENT_ID"
    clientSecret <- liftM (fromMaybe "") $ lookupEnv "DDB_TRANSLATE_CLIENT_SECRET"

    if clientId /= "" && clientSecret /= "" then
        sourceChan inChan
            =$= takeIRCMessage
            =$= CL.filter isTranslateCommand
            =$= CL.mapM_ (handleTranslateCommand manager (clientId, clientSecret) oauth outChan)
            $$ CL.sinkNull
    else
        putStrLn "ERROR: No client ID or secret for translations"

    where
        isTranslateCommand (IRC.Message _ "PRIVMSG" (_:s:[])) | "!x-" `B.isPrefixOf` s = True
        isTranslateCommand _ = False

handleTranslateCommand :: MonadIO m => HTTP.Manager -> (String, String) -> MVar (Maybe (B.ByteString, UTCTime)) -> Chan OutMessage -> IRC.Message -> m ()
handleTranslateCommand manager client oauth outChan m
        | (Just target)           <- maybeGetPrivMsgReplyTarget m
        , (Just (from, to, text)) <- parseCommandString m
        = liftIO $ void $ async (handleTranslate outChan manager client target from to text oauth)
    where
        parseCommandString m' | (_:s:[]) <- IRC.msg_params m'
                              , "!x-" `B.isPrefixOf` s
                              = case parse command "" (UB.toString s) of
                                    Left _  -> Nothing
                                    Right c -> Just c
        parseCommandString _  = Nothing

        command :: Parser (Maybe String, String, String)
        command = do
            _ <- string "!x-"
            from <- fmap Just (try (count 2 letter <* char '-')) <|> return Nothing
            to <- count 2 letter <* char ' '
            text <- many1 anyToken <* eof
            return (from, to, text)

handleTranslateCommand _ _ _ _ _ = return ()

getNewAccessToken :: HTTP.Manager -> (String, String) -> IO (B.ByteString, Int)
getNewAccessToken manager (clientId, clientSecret) = do
    let url    = "https://datamarket.accesscontrol.windows.net/v2/OAuth2-13"
        scope  = "http://api.microsofttranslator.com"
        client = B8.pack clientId
        secret = B8.pack clientSecret

    -- Catch all exceptions and print something but then rethrow them
    handle (\(SomeException e) -> putStrLn ("Exception while getting translation access token: " ++ show e) >> throwIO e) $ do
        baseReq <- HTTP.parseUrl url
        let req = HTTP.urlEncodedBody [ ("client_id", client)
                                      , ("client_secret", secret)
                                      , ("scope", scope)
                                      , ("grant_type", "client_credentials")
                                      ]
                                      baseReq

        resp <- HTTP.httpLbs req manager

        let Just oauth = decode (HTTP.responseBody resp) :: Maybe OAuth

        return (B8.pack . oAuthAccessToken $ oauth, fromMaybe 0 $ oAuthExpiresIn oauth)

getAccessToken :: HTTP.Manager -> (String, String) -> Maybe (B.ByteString, UTCTime) -> IO (Maybe (B.ByteString, UTCTime), B.ByteString)
getAccessToken manager client oauth = do
    currentTime <- getCurrentTime

    -- If the token will expire in less than 10s (or is expired), we need a new one
    let oauth' = oauth >>= (\v@(_, oldTime) -> if oldTime >= addUTCTime 10 currentTime then Just v else Nothing)

    case oauth' of
        Nothing -> do
                     (accessToken, expiresIn) <- getNewAccessToken manager client
                     currentTime' <- getCurrentTime
                     return (Just (accessToken, addUTCTime (fromIntegral expiresIn) currentTime'), accessToken)
        Just v@(accessToken, _) -> return (Just v, accessToken)

handleTranslate :: Chan OutMessage -> HTTP.Manager -> (String, String) -> B.ByteString -> Maybe String -> String -> String -> MVar (Maybe (B.ByteString, UTCTime)) -> IO ()
handleTranslate outChan manager client target from to text oauth = void $ runMaybeT $ do

    accessToken <- liftIO $ modifyMVar oauth (getAccessToken manager client)

    let url = "http://api.microsofttranslator.com/v2/Http.svc/Translate"

    -- Catch all exceptions and print something but then rethrow them
    resp <- liftIO $ handle (\(SomeException e) -> putStrLn ("Exception while getting translation: " ++ show e) >> throwIO e) $ do
        baseReq <- HTTP.parseUrl url
        let params = [ ("text", Just (UB.fromString text))
                     , ("to", Just (UB.fromString to))
                     , ("contentType", Just "text/plain")
                     ]
            params' = params ++ maybe [] (\f -> [("from", Just (UB.fromString f))]) from
            req = HTTP.setQueryString params'
                                       (baseReq { HTTP.requestHeaders = ("Authorization", "Bearer " `B.append` accessToken)
                                                            : HTTP.requestHeaders baseReq
                                                }
                                       )

        HTTP.httpLbs req manager

    let Just translation = getXMLString . UB.toString . BL.toStrict . HTTP.responseBody $ resp

    liftIO $ writeChan outChan (OutIRCMessage $ translationMessage (UB.fromString translation))

    where
        translationMessage translation = IRC.Message { IRC.msg_prefix = Nothing
                                                     , IRC.msg_command = "PRIVMSG"
                                                     , IRC.msg_params = [target, translation]
                                                     }

        getXMLString = fmap XML.strContent . headMay . filter ((stringElementName ==) . XML.elName) . XML.onlyElems . XML.parseXML
            where
                stringElementName = XML.QName "string" (Just "http://schemas.microsoft.com/2003/10/Serialization/") Nothing

