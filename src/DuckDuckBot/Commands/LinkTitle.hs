module DuckDuckBot.Commands.LinkTitle (
  linkTitleCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import Data.List
import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as UB

import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as TC

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTPH
import qualified Network.IRC as IRC

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Maybe
import Control.Concurrent
import Control.Concurrent.Async hiding (link)
import Control.Exception

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

import Safe

linkTitleCommandHandlerMetadata :: MessageHandlerMetadata
linkTitleCommandHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName = "link handler",
    messageHandlerMetadataCommands = [],
    messageHandlerMetadataHandler = linkTitleCommandHandler
}

linkTitleCommandHandler :: MessageHandler
linkTitleCommandHandler inChan outChan = do
    nick <- asks messageHandlerEnvNick
    manager <- asks messageHandlerEnvHttpManager
    sourceChan inChan
        =$= takeIRCMessage
        =$= CL.mapM_ (handleMessage nick manager outChan)
        $$ CL.sinkNull

handleMessage :: MonadIO m => String -> HTTP.Manager -> Chan OutMessage -> IRC.Message -> m ()
handleMessage nick manager outChan m@(IRC.Message (Just (IRC.NickName n _ _)) "PRIVMSG" [_, s])
        | (Just link)   <- extractedLink
        , (Just target) <- maybeGetPrivMsgReplyTarget m
        = liftIO $ void $ async (handleLink outChan manager target link)
    where
        extractedLink = if nick /= UB.toString n then
                            headMay . filter isLink . words . UB.toString $ s
                        else
                            Nothing

        isLink w = "http://" `isInfixOf` w' || "https://" `isInfixOf` w'
            where w' = map toLower w

handleMessage _ _ _ _ = return ()

handleLink :: Chan OutMessage -> HTTP.Manager -> B.ByteString -> String -> IO ()
handleLink outChan manager target link = void $ runMaybeT $ do
    content <- liftMaybe <=< liftIO $ getContent manager link
    let tags = parseTags content
    title <- liftMaybe $ getTitle tags
    void $ liftIO $ writeChan outChan $ OutIRCMessage $ generateMessage title

    where
        getTitle = fmap (T.strip . filterControl) . headDef Nothing . fmap maybeTagText . getTitleBlock . getHeadBlock . getHtmlBlock
        getBlock name = takeWhile (not . tagCloseNameLit name) . drop 1 . dropWhile (not . tagOpenNameLit name)
        getHtmlBlock = getBlock "html"
        getHeadBlock = getBlock "head"
        getTitleBlock = getBlock "title"
        filterControl = T.filter isPrint . T.map (\c -> if isSpace c then ' ' else c)

        generateMessage title = IRC.Message {  IRC.msg_prefix = Nothing,
                                               IRC.msg_command = "PRIVMSG",
                                               IRC.msg_params = [target, reply]
                                            }
                                where
                                    reply = UB.fromString $ "Title: " ++ T.unpack title ++ " (" ++ link ++ ")"

getContent :: HTTP.Manager -> String -> IO (Maybe T.Text)
getContent m url =
    -- Catch all exceptions here and return nothing
    -- Better do nothing than crashing when we can't do the HTTP request
    handle (\(SomeException e) -> putStrLn ("Exception while handling link request \"" ++ url ++ "\": " ++ show e) >> return Nothing) $ runMaybeT $ do
        baseReq <- HTTP.parseUrl url
        let headers = (HTTPH.hConnection, "Keep-Alive") : (HTTPH.hAccept, "text/html") : ("Accept-Charset", "utf8, *") : ("Accept-Language", "en, *;q=0.1") : HTTP.requestHeaders baseReq
            req  = baseReq { HTTP.requestHeaders=headers }

        (charset, body) <- liftMaybe <=< liftIO $ HTTP.withResponse req m (readChunks (100 * 1024))
        conv <- liftIO $ TC.open (UB.toString charset) (Just True)

        return $ TC.toUnicode conv body

readChunks :: Int -> HTTP.Response HTTP.BodyReader -> IO (Maybe (B.ByteString, B.ByteString))
readChunks limit resp = runMaybeT $ do
    (_, t) <- (liftMaybe . find ((== HTTPH.hContentType) . fst) . HTTP.responseHeaders) resp
    unless ("text/html" `B.isPrefixOf` t) $ fail "No HTML"

    let t' = (snd . B.breakSubstring "charset=") t
        charset = if t' == B.empty then
                    "utf-8"
                  else
                    BC.takeWhile (\a -> a /= ';' && a /= ' ') . B.drop (length ("charset=" :: String)) $ t'

    body <- liftMaybe <=< liftIO $ readChunks' [] 0
    return (charset, body)

    where
        readChunks' chunks l = do
            chunk <- (HTTP.brRead . HTTP.responseBody) resp
            if B.null chunk then
                return $ combineChunks chunks
            else do
                let chunks' = chunk : chunks
                    l' = l + B.length chunk
                if l' > limit then
                    return $ combineChunks chunks'
                else
                    readChunks' chunks' l'

        combineChunks = Just . B.concat . reverse
