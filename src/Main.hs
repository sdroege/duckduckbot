module Main (main) where

import DuckDuckBot.Types
import DuckDuckBot.Config
import DuckDuckBot.Utils

import DuckDuckBot.Commands.Ping
import DuckDuckBot.Commands.Duck
import DuckDuckBot.Commands.Ddg

import Data.Char
import Data.Maybe
import Data.List
import qualified Data.ByteString.UTF8 as UB
import qualified Data.ByteString as B

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent

import qualified Network.IRC as IRC

messageHandlers :: [MessageHandlerMetadata]
messageHandlers = [pingCommandHandlerMetadata, duckCommandHandlerMetadata, ddgCommandHandlerMetadata]

-- Run everything. We run the read loop in the main thread
-- and everything else in other threads.
--
-- Exceptions from the read and write loops are supposed to get
-- here and cause the application to stop, and the same goes for
-- all unrecoverable exceptions from command handlers
main :: IO ()
main = bracket setup shutdown run

setup :: IO Env
setup = do
    config <- getConfig
    connection <- newConnection (cfgServer config) (cfgPort config) (cfgUseSsl config)

    inChan  <- newChan :: IO (Chan InMessage)
    outChan <- newChan :: IO (Chan OutMessage)

    let env = Env {
        envConfig=config,
        envConnection=connection,
        envInChan=inChan,
        envOutChan=outChan
    }

    return env

shutdown :: Env -> IO ()
shutdown env = closeConnection (envConnection env)

run :: Env -> IO ()
run env = do
    let connection = envConnection env
        inChan = envInChan env
        outChan = envOutChan env
        config = envConfig env

    _ <- liftIO . forkIO $ runReaderT (writeLoop outChan connection) env

    -- Start all message handlers here
    let messageHandlerEnv   = MessageHandlerEnv { messageHandlerEnvNick    = cfgNick config,
                                                  messageHandlerEnvChannel = cfgChannel config
                                                }
        runMessageHandler m = do
                                 mChan <- dupChan inChan
                                 runReaderT (m mChan outChan) messageHandlerEnv
    mapM_ (liftIO . forkIO . runMessageHandler . messageHandlerMetadataHandler) messageHandlers

    -- Special handler for !help without arguments
    _ <- liftIO $ forkIO $ runMessageHandler helpCommandHandler

    runReaderT (readLoop connection inChan) env

--
-- Reads data from the handle, converts them to IRC.Messages
-- and writes them to the envInChan for all command handlers
-- to consume them
--
readLoop :: Connection -> Chan InMessage -> EnvReader IO ()
readLoop con chan = runConnectionReader con (messageReader chan)

--
-- Reads IRC messages from the socket and passes them to all
-- the command handler
--
-- We don't catch any exceptions here. If writing to the
-- connection fails we will stop the application
--
messageReader :: Chan InMessage -> ConnectionReader (EnvReader IO) ()
messageReader chan = forever $ do
    msg <- getMessage
    liftIO $ writeChan chan (InIRCMessage msg)

--
-- Reads Messages from the envOutChan and does whatever
-- is necessary to handle them, i.e. write IRC.Messages
-- to the handle or quit or ...
--
-- We don't catch any exceptions here. If writing to the
-- connection fails we will stop the application
--
writeLoop :: Chan OutMessage -> Connection -> EnvReader IO ()
writeLoop chan con = do
    config <- asks envConfig
    -- Send initial commands
    let sendNickCommand = write "NICK" [cfgNick config]
        sendUserCommand = write "USER" [cfgNick config, "0", "*", "duck duck bot"]
        sendNickServPassword = case config of
                                    (Config { cfgNickServPassword=Just pw}) -> write "PRIVMSG" ["NickServ", "IDENTIFY " ++ pw]
                                    _                                       -> return ()
        sendJoinCommand = write "JOIN" [cfgChannel config]
        write command params = liftIO $ putMessageConnection con (IRC.Message Nothing command (convertParams params))
        convertParams = map UB.fromString

    sendNickCommand
    sendUserCommand
    sendNickServPassword
    sendJoinCommand

    forever $ do
        msg <- liftIO $ readChan chan
        -- Catch exceptions that might occur from evaluation
        -- a lazy, non-evaluated message here
        evMsg <- liftIO (try (evaluate msg) :: IO (Either SomeException OutMessage))
        case evMsg of
            Right (OutIRCMessage m) -> liftIO $ putMessageConnection con m
            Left e                  -> liftIO $ print ("Exception in writeLoop: " ++ show e)


--
-- Special handler for the !help command without parameters
--
helpCommandHandler :: MessageHandler
helpCommandHandler cIn cOut = forever $ do
    msg <- liftIO $ readChan cIn
    case msg of
        InIRCMessage m | isHelpCommand m -> handleHelp m
        _                                -> return ()
    where
        isHelpCommand msg = command == "PRIVMSG" && length params == 2 && s == "!help"
                            where
                                command = IRC.msg_command msg
                                params = IRC.msg_params msg
                                s = head (tail params)
        handleHelp msg = when (target /= B.empty) $ sendHelp target
                         where
                             target = getPrivMsgReplyTarget msg
        sendHelp target = liftIO $ writeChan cOut (helpMessage target)
        helpMessage target = OutIRCMessage IRC.Message  {   IRC.msg_prefix = Nothing,
                                                            IRC.msg_command = "PRIVMSG",
                                                            IRC.msg_params = [target, helpString]
                                                        }
                             where
                                helpString = UB.fromString $ "Available commands: " ++ commands -- ++ ". Try !help COMMAND"
                                commands = mapIntercalate (handlerCommands . messageHandlerMetadataCommands) ", " messageHandlers
                                handlerCommands = mapIntercalate id " | "
                                mapIntercalate f s l = intercalate s (mapMaybe (\a -> let r = f a in if null (dropWhile isSpace r) then Nothing else Just r) l)

