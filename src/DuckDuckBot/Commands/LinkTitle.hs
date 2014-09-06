module DuckDuckBot.Commands.LinkTitle (
  linkTitleCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import Data.List
import Data.Char
import Data.String.Utils (strip)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTPH
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.IRC as IRC

import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Control.Concurrent
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
linkTitleCommandHandler = messageHandlerLoop run handleMessage

type LinkTitleReader = ReaderT HTTP.Manager

run :: LinkTitleReader (MessageHandlerEnvReader IO) () -> MessageHandlerEnvReader IO ()
run l = do
    manager <- liftIO $ HTTP.newManager HTTPS.tlsManagerSettings
    void $ runReaderT l manager
    liftIO $ HTTP.closeManager manager

handleMessage :: IRC.Message -> MessageHandlerSendMessage -> LinkTitleReader (MessageHandlerEnvReader IO) ()
handleMessage msg send = do
    manager <- ask
    nick <- lift $ asks messageHandlerEnvNick
    case msg of
        (IRC.Message (Just (IRC.NickName n _ _)) "PRIVMSG" (_:s:[])) | containsLink nick n s -> when (target /= B.empty && link /= empty) $
                                                                                                    liftIO $ void $ forkIO (handleLink send manager target link)
                                                                                                where
                                                                                                    target = getPrivMsgReplyTarget msg
                                                                                                    link = extractLink s
                                                                                                    extractLink = headDef "" . filter isLink . words . UB.toString
        _                                                                                     -> return ()
    where
        containsLink n n' s = n /= n'' && isLink s'
            where s' = UB.toString s
                  n'' = UB.toString n'
        isLink s = "http://" `isInfixOf` s' || "https://" `isInfixOf` s'
            where s' = map toLower s

handleLink :: MessageHandlerSendMessage -> HTTP.Manager -> B.ByteString -> String -> IO ()
handleLink send manager target link = do
    maybeContent <- getContent manager link
    case maybeContent of
        Just content -> do
            let tags = parseTags content
            let title = getTitle tags
            case title of
                Just s -> send $ generateMessage s
                _      -> return ()
        _            -> return ()
    where
        getTitle = fmap strip . headDef Nothing . fmap maybeTagText . getTitleBlock . getHeadBlock . getHtmlBlock
        getBlock name = takeWhile (not . tagCloseNameLit name) . drop 1 . dropWhile (not . tagOpenNameLit name)
        getHtmlBlock = getBlock "html"
        getHeadBlock = getBlock "head"
        getTitleBlock = getBlock "title"

        generateMessage title = IRC.Message {  IRC.msg_prefix = Nothing,
                                               IRC.msg_command = "PRIVMSG",
                                               IRC.msg_params = [target, reply]
                                            }
                                where
                                    reply = UB.fromString $ "Title: " ++ title ++ " (" ++ link ++ ")"

getContent :: HTTP.Manager -> String -> IO (Maybe String)
getContent m url =
    -- Catch all exceptions here and return nothing
    -- Better do nothing than crashing when we can't do the HTTP request
    handle (\(SomeException e) -> print ("Exception while handling link request \"" ++ url ++ "\": " ++ show e) >> return Nothing) $ do
        baseReq <- HTTP.parseUrl url
        let headers = (HTTPH.hConnection, "Keep-Alive") : HTTP.requestHeaders baseReq
            req  = baseReq { HTTP.requestHeaders=headers }

            readChunks limit resp = readChunks' [] 0
                                    where readChunks' chunks l = do
                                              chunk <- (HTTP.brRead . HTTP.responseBody) resp
                                              if B.null chunk then
                                                  return $ combineChunks chunks
                                              else
                                                  do
                                                      let chunks' = chunk : chunks
                                                          l' = l + B.length chunk

                                                      if l' > limit then
                                                          return $ combineChunks chunks'
                                                      else
                                                          readChunks' chunks' l'
                                          combineChunks = Just . B.concat . reverse

        maybeBody <- HTTP.withResponse req m (readChunks (100 * 1024))

        return $ fmap UB.toString maybeBody


