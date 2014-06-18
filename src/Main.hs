module Main (main) where

import DuckDuckBot.Types
import DuckDuckBot.Config

import DuckDuckBot.Commands.Ping
import DuckDuckBot.Commands.Duck
import DuckDuckBot.Commands.Ddg

import qualified Data.ByteString.UTF8 as UB

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent

import qualified Network.IRC as IRC

messageHandlers :: [MessageHandler]
messageHandlers = [pingCommandHandler, duckCommandHandler, ddgCommandHandler]

main :: IO ()
main = bracket setup shutdown loop

setup :: IO Env
setup = do
    config <- defaultConfig
    connection <- newConnection (cfgServer config) (cfgPort config) (cfgUseSsl config)

    inChan  <- newChan :: IO (Chan InMessage)
    outChan <- newChan :: IO (Chan OutMessage)

    let env = Env {
        envConfig=config,
        envConnection=connection,
        envInChan=inChan,
        envOutChan=outChan
    }

    _ <- liftIO . forkIO $ runReaderT (writeLoop outChan connection) env

    -- Start all message handlers here
    let messageHandlerEnv   = MessageHandlerEnv { messageHandlerEnvNick    = cfgNick config,
                                                  messageHandlerEnvChannel = cfgChannel config
                                                }
        runMessageHandler m = do
                                 mChan <- dupChan inChan
                                 runReaderT (m mChan outChan) messageHandlerEnv
    mapM_ (liftIO . forkIO . runMessageHandler) messageHandlers

    return env

shutdown :: Env -> IO ()
shutdown env = closeConnection (envConnection env)

loop :: Env -> IO ()
loop st = catch (runReaderT run st) (\(SomeException e) -> (putStrLn $ "Exception: " ++ show e))

--
-- Send initial commands and run the readLoop on the handle
--
run :: EnvReader IO ()
run = do
    config <- asks envConfig
    outChan <- asks envOutChan
    sendNickCommand config outChan
    sendUserCommand config outChan
    sendNickServPassword config outChan
    sendJoinCommand config outChan
    connection <- asks envConnection
    inChan <- asks envInChan
    readLoop connection inChan
    where
        sendNickCommand config = write "NICK" [cfgNick config]
        sendUserCommand config = write "USER" [cfgNick config, "0", "*", "duck duck bot"]
        sendNickServPassword (Config { cfgNickServPassword=Just pw}) c = write "PRIVMSG" ["NickServ", "IDENTIFY " ++ pw] c
        sendNickServPassword (Config { cfgNickServPassword=Nothing}) _ = return ()
        sendJoinCommand config = write "JOIN" [cfgChannel config]
        write command params chan = liftIO $ writeChan chan $ OutIRCMessage (IRC.Message Nothing command (convertParams params))
        convertParams = map UB.fromString

--
-- Reads data from the handle, converts them to IRC.Messages
-- and writes them to the envInChan for all command handlers
-- to consume them
--
readLoop :: Connection -> Chan InMessage -> EnvReader IO ()
readLoop con chan = runConnectionReader con (messageReader chan)

messageReader :: Chan InMessage -> ConnectionReader (EnvReader IO) ()
messageReader chan = forever $ do
    msg <- getMessage
    case msg of
        (Just s)    -> liftIO $ writeChan chan (InIRCMessage s)
        Nothing     -> return ()

--
-- Reads Messages from the envOutChan and does whatever
-- is necessary to handle them, i.e. write IRC.Messages
-- to the handle or quit or ...
--
writeLoop :: Chan OutMessage -> Connection -> EnvReader IO ()
writeLoop chan con = forever $ do
    msg <- liftIO $ readChan chan
    case msg of
        OutIRCMessage m -> liftIO $ putMessageConnection con m

