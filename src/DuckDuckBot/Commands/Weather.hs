module DuckDuckBot.Commands.Weather (
  weatherCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils
import DuckDuckBot.Compat

import System.Environment

import Data.Char
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import Data.Aeson

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.IRC as IRC

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Trans.Maybe
import Control.Exception

data OwmReply = OwmReply
    { owmReplyName :: String
    , owmReplySys :: OwmSys
    , owmReplyMain :: OwmMain
    , owmReplyWind :: OwmWind
    , owmReplyWeather :: [OwmWeather]
    } deriving (Show, Eq)

data OwmSys = OwmSys
    { owmSysCountry :: String
    } deriving (Show, Eq)

data OwmMain = OwmMain
    { owmMainTemp :: Double
    , owmMainTempMin :: Double
    , owmMainTempMax :: Double
    , owmMainHumidity :: Double
    } deriving (Show, Eq)

data OwmWind = OwmWind
    { owmWindSpeed :: Double
    , owmWindGust :: Maybe Double
    } deriving (Show, Eq)

data OwmWeather = OwmWeather
    { owmWeatherMain :: String
    , owmWeatherDescription :: String
    } deriving (Show, Eq)

instance FromJSON OwmReply where
    parseJSON (Object v) = OwmReply
                                <$> v .: "name"
                                <*> v .: "sys"
                                <*> v .: "main"
                                <*> v .: "wind"
                                <*> v .: "weather"
    parseJSON _          = mzero

instance FromJSON OwmSys where
    parseJSON (Object v) = OwmSys
                                <$> v .: "country"
    parseJSON _          = mzero

instance FromJSON OwmMain where
    parseJSON (Object v) = OwmMain
                                <$> v .: "temp"
                                <*> v .: "temp_min"
                                <*> v .: "temp_max"
                                <*> v .: "humidity"
    parseJSON _          = mzero

instance FromJSON OwmWind where
    parseJSON (Object v) = OwmWind
                                <$> v .: "speed"
                                <*> v .:? "gust"
    parseJSON _          = mzero

instance FromJSON OwmWeather where
    parseJSON (Object v) = OwmWeather
                                <$> v .: "main"
                                <*> v .: "description"
    parseJSON _          = mzero

weatherCommandHandlerMetadata :: MessageHandlerMetadata
weatherCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "weather",
    messageHandlerMetadataCommands = ["!weather"],
    messageHandlerMetadataHandler = weatherCommandHandler
}

weatherCommandHandler :: MessageHandler
weatherCommandHandler inChan outChan = liftIO $ HTTP.withManager HTTPS.tlsManagerSettings $ \manager -> do
    appId <- liftM (fromMaybe "") $ lookupEnv "DDB_WEATHER_APP_ID"

    if appId /= "" then
        sourceChan inChan
            =$= takeIRCMessage
            =$= CL.filter isWeatherCommand
            =$= CL.mapM_ (handleWeatherCommand manager appId outChan)
            $$ CL.sinkNull
    else
        putStrLn "ERROR: No app ID or secret for weather"

    where
        isWeatherCommand = isPrivMsgCommand "weather"

handleWeatherCommand :: MonadIO m => HTTP.Manager -> String -> Chan OutMessage -> IRC.Message -> m ()
handleWeatherCommand manager appId outChan m
        | (Just target) <- maybeGetPrivMsgReplyTarget m
        , (Just location)  <- parseWeatherString m
        = liftIO $ void $ async (handleWeather outChan manager appId target location)
    where
        parseWeatherString m' | (_:s:[]) <- IRC.msg_params m'
                            = case extractLocation s of
                                q | q == B.empty -> Nothing
                                  | otherwise    -> Just q
        parseWeatherString _  = Nothing

        extractLocation = B.dropWhile isSpaceB . B.drop (length ("!weather" :: String))
        isSpaceB = (== fromIntegral (ord ' '))

handleWeatherCommand _ _ _ _ = return ()

handleWeather :: Chan OutMessage -> HTTP.Manager -> String -> B.ByteString -> B.ByteString -> IO ()
handleWeather outChan manager appId target location = void $ runMaybeT $ do
    let url = "http://api.openweathermap.org/data/2.5/weather"

    -- Catch all exceptions and print something but then rethrow them
    resp <- liftIO $ handle (\(SomeException e) -> putStrLn ("Exception while getting translation: " ++ show e) >> throwIO e) $ do
        baseReq <- HTTP.parseUrl url
        let params = [ ("units", Just "metric")
                     , ("q", Just location)
                     ]
            req = HTTP.setQueryString params
                                       (baseReq { HTTP.requestHeaders = ("x-api-key", UB.fromString appId)
                                                            : HTTP.requestHeaders baseReq
                                                }
                                       )

        HTTP.httpLbs req manager

    let reply = generateReply (decode (HTTP.responseBody resp) :: Maybe OwmReply)
    liftIO $ writeChan outChan (OutIRCMessage $ replyMessage (UB.fromString reply))

    where
        replyMessage text = IRC.Message {  IRC.msg_prefix = Nothing,
                                           IRC.msg_command = "PRIVMSG",
                                           IRC.msg_params = [target, text]
                                        }

        generateReply (Just (OwmReply name
                                (OwmSys country)
                                (OwmMain temp tempMin tempMax humidity)
                                (OwmWind speed gust)
                                (OwmWeather weather description:_))) = "Weather for " ++ name ++ " (" ++ country ++ "): "
                                                                         ++ weather ++ " (" ++ description ++ "), Temperature: "
                                                                         ++ show temp ++ "°C (from " ++ show tempMin ++ "°C to "
                                                                         ++ show tempMax ++ "°C), Humidity: " ++ show humidity
                                                                         ++ "%, Wind speed: " ++ show speed ++ "km/s"
                                                                         ++ maybe "." (\g -> ", gust: " ++ show g ++ "km/s.") gust

        generateReply _                                                = "Failed to get weather for " ++ UB.toString location
