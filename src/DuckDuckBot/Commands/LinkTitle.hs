module DuckDuckBot.Commands.LinkTitle (
  linkTitleCommandHandlerMetadata
) where

import DuckDuckBot.Types
import DuckDuckBot.Utils

import Data.List
import Data.Char
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

import Text.XML.HXT.Core hiding (when)

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
                                                                                                    extractLink = headDef "" . filter (isLink) . words . UB.toString
        _                                                                                     -> return ()
    where
        containsLink n n' s = n /= n'' && isLink s'
            where s' = UB.toString s
                  n'' = UB.toString n'
        isLink s = ("http://" `isInfixOf` s' || "https://" `isInfixOf` s')
            where s' = map toLower s

handleLink :: MessageHandlerSendMessage -> HTTP.Manager -> B.ByteString -> String -> IO ()
handleLink send manager target link = do
    maybeContent <- getContent manager link
    case maybeContent of
        Just content -> do
            title <- runX (parseHTML content >>> getTitle)
            when (title /= []) $ do
                let msg = generateMessage (intercalate "\n" title)
                send msg
        _            -> return ()
    where
        parseHTML = readString [ withParseHTML yes, withWarnings no, withValidate no ]
        getTitle = getChildren >>> hasName "html" /> hasName "head" /> hasName "title" /> getText

        generateMessage title = IRC.Message {  IRC.msg_prefix = Nothing,
                                               IRC.msg_command = "PRIVMSG",
                                               IRC.msg_params = [target, reply]
                                            }
                                where
                                    reply = UB.fromString $ "Title: " ++ title ++ " (" ++ link ++ ")"

getContent :: HTTP.Manager -> String -> IO (Maybe String)
getContent m url = do
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
                                                  return $ (Just . B.concat . reverse) chunks
                                              else
                                                  do
                                                      let chunks' = chunk : chunks
                                                          l' = l + B.length chunk

                                                      if l' > limit then
                                                          return Nothing
                                                      else
                                                          readChunks' chunks' l'

        maybeBody <- HTTP.withResponse req m (readChunks (2 * 1024 * 1024))

        return $ fmap UB.toString maybeBody


