module DuckDuckBot.Commands.UrbanDictionary (
  urbanDictionaryCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils
import DuckDuckBot.Compat

import System.Environment

import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import Data.Aeson

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Client as HTTP
import qualified Network.IRC as IRC

import Control.Applicative
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Concurrent.STM.TMChan
import Control.Concurrent.Async
import Control.Monad.Trans.Maybe
import Control.Exception

data UDReply = UDReply
    { udList :: [UDEntry]
    } deriving (Show, Eq)

data UDEntry = UDEntry
    { udWord :: String
    , udDefId :: Int
    , udDefinition :: String
    } deriving (Show, Eq)

instance FromJSON UDReply where
    parseJSON (Object v) = UDReply
                                <$> v .: "list"
    parseJSON _          = mzero

instance FromJSON UDEntry where
    parseJSON (Object v) = UDEntry
                                <$> v .: "word"
                                <*> v .: "defid"
                                <*> v .: "definition"
    parseJSON _          = mzero

urbanDictionaryCommandHandlerMetadata :: MessageHandlerMetadata
urbanDictionaryCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "urbanDictionary",
    messageHandlerMetadataCommands = ["!udict"],
    messageHandlerMetadataHandler = urbanDictionaryCommandHandler
}

urbanDictionaryCommandHandler :: MessageHandler
urbanDictionaryCommandHandler inChan outChan = do
    manager <- asks messageHandlerEnvHttpManager
    sourceChan inChan
        =$= takeIRCMessage
        =$= CL.filter isUDCommand
        =$= CL.mapM_ (handleUDCommand manager outChan)
        $$ CL.sinkNull

    where
        isUDCommand = isPrivMsgCommand "udict"

handleUDCommand :: MonadIO m => HTTP.Manager -> TMChan OutMessage -> IRC.Message -> m ()
handleUDCommand manager outChan m
        | (Just target) <- maybeGetPrivMsgReplyTarget m
        , (Just term)  <- parseUDString m
        = liftIO $ void $ async (handleUD outChan manager target term)
    where
        parseUDString m' | [_, s] <- IRC.msg_params m'
                            = case extractTerm s of
                                q | q == B.empty -> Nothing
                                  | otherwise    -> Just q
        parseUDString _  = Nothing

        extractTerm = B.dropWhile isSpaceB . B.drop (length ("!udict" :: String))
        isSpaceB = (== fromIntegral (ord ' '))

handleUDCommand _ _ _ = return ()

handleUD :: TMChan OutMessage -> HTTP.Manager -> B.ByteString -> B.ByteString -> IO ()
handleUD outChan manager target term = void $ runMaybeT $ do
    let url = "http://api.urbandictionary.com/v0/define"

    -- Catch all exceptions and print something but then rethrow them
    resp <- liftIO $ handle (\(SomeException e) -> putStrLn ("Exception while getting Urban Dictionary result: " ++ show e) >> throwIO e) $ do
        baseReq <- HTTP.parseUrl url
        let params = [ ("term", Just term)
                     ]
            req = HTTP.setQueryString params baseReq

        HTTP.httpLbs req manager

    let reply = generateReply (decode (HTTP.responseBody resp) :: Maybe UDReply)
    liftIO . atomically $ writeTMChan outChan (OutIRCMessage $ replyMessage (UB.fromString reply))

    where
        replyMessage text = IRC.Message {  IRC.msg_prefix = Nothing,
                                           IRC.msg_command = "PRIVMSG",
                                           IRC.msg_params = [target, text]
                                        }

        generateReply (Just (UDReply (UDEntry word defId definition:_))) = shortenedDefinition
                                                                             ++ " (http://urbanup.com/" ++ show defId ++ ")"
                                                                           where
                                                                             maxIrcLen = 400
                                                                             shortenedDefinition = take maxIrcLen formattedDefinition ++ dots
                                                                                where
                                                                                    dots = if length formattedDefinition > maxIrcLen then "…" else ""

                                                                             formattedDefinition = word ++ ": "
                                                                                                     ++ cleanedDefinition
                                                                             cleanedDefinition = filter isPrint . map (\c -> if isSpace c then ' ' else c) . filter (/= '\r') $ definition

        generateReply _                                                    = "No results for " ++ UB.toString term
