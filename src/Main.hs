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

    authUser <- newMVar (Nothing :: Maybe IRC.Prefix)

    let env = Env {
        envConfig=config,
        envConnection=connection,
        envInChan=inChan,
        envOutChan=outChan,
        envAuthUser=authUser
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
        authUser = envAuthUser env

    threads <- newMVar []
    let forkThread io = do
                            mvar <- newEmptyMVar
                            ts   <- takeMVar threads
                            putMVar threads (mvar:ts)
                            void (forkFinally io (\_ -> putMVar mvar ()))
        waitForThreads = do
                            ts   <- takeMVar threads
                            case ts of
                                []    -> return ()
                                m:ms  -> do
                                            putMVar threads ms
                                            takeMVar m
                                            waitForThreads

    liftIO . forkThread $ runReaderT (writeLoop outChan connection) env

    -- Start all message handlers here
    let messageHandlerEnv   = MessageHandlerEnv { messageHandlerEnvNick    = cfgNick config,
                                                  messageHandlerEnvChannel = cfgChannel config,
                                                  messageHandlerEnvIsAuthUser = isAuthUser authUser
                                                }
        runMessageHandler m = do
                                 mChan <- dupChan inChan
                                 runReaderT (m mChan outChan) messageHandlerEnv
    mapM_ (liftIO . forkThread . runMessageHandler . messageHandlerMetadataHandler) messageHandlers

    -- Special handler for !help without arguments
    liftIO $ forkThread $ runMessageHandler helpCommandHandler

    -- Special handler for !auth
    when (isJust (cfgAuthPassword config)) $
        liftIO $ forkThread $ runMessageHandler (authCommandHandler (fromJust (cfgAuthPassword config)) authUser)

    runReaderT (readLoop messageHandlerEnv connection inChan outChan) env

    waitForThreads

--
-- Reads data from the handle, converts them to IRC.Messages
-- and writes them to the envInChan for all command handlers
-- to consume them
--
readLoop :: MessageHandlerEnv -> Connection -> Chan InMessage -> Chan OutMessage -> EnvReader IO ()
readLoop env con inChan outChan = runConnectionReader con (messageReader env inChan outChan)

--
-- Reads IRC messages from the socket and passes them to all
-- the command handler
--
-- We don't catch any exceptions here. If writing to the
-- connection fails we will stop the application
--
messageReader :: MessageHandlerEnv -> Chan InMessage -> Chan OutMessage -> ConnectionReader (EnvReader IO) ()
messageReader env inChan outChan = untilFalse $ do
    msg <- getMessage
    liftIO $ writeChan inChan (InIRCMessage msg)

    let isQuitCommand = command == "PRIVMSG" && length params == 2 && s == "!quit"
                        where
                            command = IRC.msg_command msg
                            params = IRC.msg_params msg
                            s = head (tail params)
        prefix = IRC.msg_prefix msg

    if isQuitCommand && isJust prefix then
        do
            a <- liftIO $ messageHandlerEnvIsAuthUser env (fromJust prefix)
            if a then
                do
                    liftIO $ writeChan inChan Quit
                    let quitMessage = IRC.Message Nothing "QUIT" []
                    liftIO $ writeChan outChan (OutIRCMessage quitMessage)
                    return False
            else
                return True
    else
        return True

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

    untilFalse $ do
        msg <- liftIO $ readChan chan
        -- Catch exceptions that might occur from evaluation
        -- a lazy, non-evaluated message here
        evMsg <- liftIO (try (evaluate msg) :: IO (Either SomeException OutMessage))
        case evMsg of
            Right (OutIRCMessage m) -> do
                                            liftIO $ putMessageConnection con m
                                            case m of
                                                m' | IRC.msg_command m' == "QUIT" -> return False
                                                _                                 -> return True
            Left e                  -> do
                                            liftIO $ print ("Exception in writeLoop: " ++ show e)
                                            return True


--
-- Special handler for the !help command without parameters
--
helpCommandHandler :: MessageHandler
helpCommandHandler cIn cOut = untilFalse $ do
    msg <- liftIO $ readChan cIn
    case msg of
        InIRCMessage m | isHelpCommand m -> handleHelp m >> return True
        Quit                             -> return False
        _                                -> return True
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

--
-- Special handler for the !auth command
--
authCommandHandler :: String -> MVar (Maybe IRC.Prefix) -> MessageHandler
authCommandHandler pw authUser cIn cOut = untilFalse $ do
    msg <- liftIO $ readChan cIn
    case msg of
        InIRCMessage m | isAuthCommand m -> handleAuth m >> return True
        Quit                             -> return False
        _                                -> return True
    where
        isAuthCommand msg = command == "PRIVMSG" && length params == 2 && "!auth " `B.isPrefixOf` s
                            where
                                command = IRC.msg_command msg
                                params = IRC.msg_params msg
                                s = head (tail params)
        handleAuth msg = do
                            let prefix = IRC.msg_prefix msg
                                params = IRC.msg_params msg
                                s = head (tail params)
                                p = UB.toString (B.drop 6 s)

                            case prefix of
                                Just (IRC.NickName n _ _) -> if p == pw then
                                                                liftIO $ swapMVar authUser prefix >> sendAuthMessage n ("Authenticated " `B.append` n)
                                                             else
                                                                sendAuthMessage n "Authentication failed"

                                _            -> return ()


        sendAuthMessage n m = liftIO $ writeChan cOut (authMessage n m)
        authMessage n m = OutIRCMessage IRC.Message {   IRC.msg_prefix = Nothing,
                                                        IRC.msg_command = "NOTICE",
                                                        IRC.msg_params = [n, m]
                                                    }

isAuthUser :: MVar (Maybe IRC.Prefix) -> IRC.Prefix -> IO Bool
isAuthUser m n = do
    a <- readMVar m
    case a of
        Just p -> return (n == p)
        _      -> return False

