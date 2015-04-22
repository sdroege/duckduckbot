module DuckDuckBot.Core
    ( run
    ) where

import Prelude hiding (catch)

import DuckDuckBot.Types
import DuckDuckBot.Utils
import DuckDuckBot.Compat

import DuckDuckBot.Commands.Ping
import DuckDuckBot.Commands.Duck
import DuckDuckBot.Commands.Ddg
import DuckDuckBot.Commands.Quotes
import DuckDuckBot.Commands.LinkTitle
import DuckDuckBot.Commands.Translate
import DuckDuckBot.Commands.Weather

import Data.Char
import Data.Maybe
import Data.List
import qualified Data.ByteString.UTF8 as UB
import qualified Data.ByteString as B
import Data.IORef

import Data.Conduit
import qualified Data.Conduit.List as CL

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent.STM.TMChan
import Control.Concurrent.Async
import Control.Exception.Base (AsyncException(UserInterrupt, ThreadKilled))

import qualified Network.IRC as IRC

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

-- Run everything. We run the read loop in the main thread
-- and everything else in other threads.
--
-- Exceptions from the read and write loops are supposed to get
-- here and cause the application to stop, and the same goes for
-- all unrecoverable exceptions from command handlers
run :: Config -> IO ()
run config = bracket (setup config) shutdown loop

messageHandlers :: [MessageHandlerMetadata]
messageHandlers = [pingCommandHandlerMetadata, duckCommandHandlerMetadata, ddgCommandHandlerMetadata, quotesCommandHandlerMetadata, linkTitleCommandHandlerMetadata, translateCommandHandlerMetadata, weatherCommandHandlerMetadata]

setup :: Config -> IO Env
setup config = do
    connection <- newConnection (cfgServer config) (cfgPort config) (cfgUseSsl config)

    inChan  <- newBroadcastTMChanIO
    outChan <- newTMChanIO

    let env = Env {
        envConfig=config,
        envConnection=connection,
        envInChan=inChan,
        envOutChan=outChan
    }

    return env

shutdown :: Env -> IO ()
shutdown env = closeConnection (envConnection env)

loop :: Env -> IO ()
loop env = bracket (HTTP.newManager HTTPS.tlsManagerSettings) HTTP.closeManager $ \httpManager -> do
    let connection = envConnection env
        inChan = envInChan env
        outChan = envOutChan env
        config = envConfig env

    authUser <- newIORef Nothing

    -- Start all message handlers here
    let messageHandlerEnv   = MessageHandlerEnv { messageHandlerEnvServer  = cfgServer config,
                                                  messageHandlerEnvNick    = cfgNick config,
                                                  messageHandlerEnvChannel = cfgChannel config,
                                                  messageHandlerEnvIsAuthUser = isAuthUser authUser,
                                                  messageHandlerEnvHttpManager = httpManager
                                                }
        runMessageHandler m = do
            mChan <- liftIO . atomically $ dupTMChan inChan
            runReaderT (m mChan outChan) messageHandlerEnv

        allHandlers = helpCommandHandler
            :  maybeToList (fmap (authCommandHandler authUser) (cfgAuthPassword config))
            ++ map messageHandlerMetadataHandler messageHandlers

        asyncCatch a = async (handle (\ThreadKilled -> return ()) a)

    writerThread <- liftIO . asyncCatch $ writeLoop (envConfig env) outChan connection

    bracket (mapM (liftIO . asyncCatch . runMessageHandler) allHandlers)
        (\threads -> mapM_ (liftIO . cancel) threads >> liftIO (cancel writerThread))
        $ \threads -> do
            liftIO $ link writerThread
            liftIO $ mapM_ link threads

            catch (readLoop messageHandlerEnv connection inChan)
                (\UserInterrupt -> liftIO . atomically $ closeTMChan inChan)
            mapM_ wait threads

            let quitMessage = IRC.Message Nothing "QUIT" []
            liftIO . atomically $ do
                writeTMChan outChan (OutIRCMessage quitMessage)
                closeTMChan outChan

            wait writerThread

--
-- Reads data from the handle, converts them to IRC.Messages
-- and writes them to the envInChan for all command handlers
-- to consume them
--
readLoop :: MessageHandlerEnv -> Connection -> TMChan InMessage -> IO ()
readLoop env con inChan =
    sourceIRCConnection con $$ handleIRCMessage =$= sinkChan inChan

    where
        handleIRCMessage = do
            msg <- await
            case msg of
                Just m -> do
                            yield $ InIRCMessage m
                            q <- shouldQuit m
                            if q then do
                                liftIO . atomically $ closeTMChan inChan
                                return ()
                            else
                                handleIRCMessage
                _      -> return ()

        shouldQuit (IRC.Message (Just prefix) "PRIVMSG" [_,"!quit"]) = liftIO $ messageHandlerEnvIsAuthUser env prefix
        shouldQuit _                                                 = return False


--
-- Reads Messages from the envOutChan and does whatever
-- is necessary to handle them, i.e. write IRC.Messages
-- to the handle or quit or ...
--
-- We don't catch any exceptions here. If writing to the
-- connection fails we will stop the application
--
writeLoop :: Config -> TMChan OutMessage -> Connection -> IO ()
writeLoop config chan con = do

    -- Send initial commands
    let initialCommands = map (\(c, p) -> IRC.Message Nothing c (map UB.fromString p)) . catMaybes $
            [ Just ("NICK", [cfgNick config])
            , Just ("USER", [cfgNick config, "0", "*", "duck duck bot"])
            , maybe Nothing (\pw -> Just ("PRIVMSG", ["NickServ", "IDENTIFY " ++ pw])) (cfgNickServPassword config)
            , Just ("JOIN", [cfgChannel config])
            ]
    CL.sourceList initialCommands
        $$ sinkIRCConnection con

    -- Write all following messages and stop on QUIT
    sourceChan chan
        $$ CL.map unwrapOutIRCMessage
        =$= sinkIRCConnection con
        where
            unwrapOutIRCMessage (OutIRCMessage m) = m

--
-- Special handler for the !help command without parameters
--
helpCommandHandler :: MessageHandler
helpCommandHandler inChan outChan =
    sourceChan inChan
        =$= takeIRCMessage
        =$= CL.mapMaybe handleHelpCommand
        $$ CL.map OutIRCMessage
        =$= sinkChan outChan

    where
        handleHelpCommand m@(IRC.Message _ "PRIVMSG" [_, "!help"])
                                | (Just target) <- maybeGetPrivMsgReplyTarget m
                                  = Just $ helpMessage target
        handleHelpCommand _       = Nothing

        helpMessage target = IRC.Message  {   IRC.msg_prefix = Nothing,
                                              IRC.msg_command = "PRIVMSG",
                                              IRC.msg_params = [target, helpString]
                                          }
                             where
                                helpString = UB.fromString $ "Available commands: " ++ commands -- ++ ". Try !help COMMAND"
                                commands = mapIntercalate (handlerCommands . messageHandlerMetadataCommands) ", " messageHandlers
                                handlerCommands = mapIntercalate id ", "
                                mapIntercalate f s l = intercalate s (mapMaybe (\a -> let r = f a in if null (dropWhile isSpace r) then Nothing else Just r) l)

--
-- Special handler for the !auth command
--
authCommandHandler :: IORef (Maybe IRC.Prefix) -> String -> MessageHandler
authCommandHandler authUser pw inChan outChan =
    sourceChan inChan
        =$= takeIRCMessage
        =$= CL.concatMapM handleAuthCommand
        $$ CL.map OutIRCMessage
        =$= sinkChan outChan

    where
        handleAuthCommand (IRC.Message (Just prefix@(IRC.NickName n _ _)) "PRIVMSG" [_, s]) | "!auth " `B.isPrefixOf` s = handleAuth prefix n s
        handleAuthCommand _                                                                                               = return []

        handleAuth prefix n s = do
                                    let p = UB.toString (B.drop (length ("!auth " :: String)) s)
                                    if p == pw then
                                        do
                                            old <- liftIO $ atomicModifyIORef' authUser (\a -> (Just prefix, a))
                                            return $ catMaybes
                                                [ Just $ authMessage n ("Authenticated " `B.append` n)
                                                , case old of
                                                    Just (IRC.NickName n' _ _) -> Just $ authMessage n' ("Authenticated " `B.append` n)
                                                    _                          -> Nothing
                                                ]
                                    else
                                        return [authMessage n "Authentication failed"]

        authMessage n m = IRC.Message {   IRC.msg_prefix = Nothing,
                                          IRC.msg_command = "NOTICE",
                                          IRC.msg_params = [n, m]
                                      }

isAuthUser :: IORef (Maybe IRC.Prefix) -> IRC.Prefix -> IO Bool
isAuthUser m n = do
    a <- readIORef m
    case a of
        Just p -> return (n == p)
        _      -> return False

