module Main (main) where

import DuckDuckBot.Types
import DuckDuckBot.Config

import DuckDuckBot.Commands.Ping

import Network
import System.IO
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import qualified Network.IRC as IRC

messageHandlers :: [MessageHandler]
messageHandlers = [pingCommandHandler]

main :: IO ()
main = bracket setup shutdown loop

setup :: IO Env
setup = do
    config <- defaultConfig
    h <- connectTo (cfgServer config) (PortNumber (fromIntegral (cfgPort config)))
    hSetBuffering h NoBuffering
    hSetEncoding h utf8

    inChan  <- newChan :: IO (Chan InMessage)
    outChan <- newChan :: IO (Chan OutMessage)

    let env = Env {
        envConfig=config,
        envHandle=h,
        envInChan=inChan,
        envOutChan=outChan
    }

    _ <- liftIO . forkIO $ runReaderT (writeLoop outChan h) env

    -- Start all message handlers here
    let pluginEnv = PluginEnv { pluginEnvNick = cfgNick config
                              }
        runMessageHandler m = do
                                 mChan <- dupChan inChan
                                 runReaderT (m mChan outChan) pluginEnv
    mapM_ (liftIO . forkIO . runMessageHandler) messageHandlers

    return env

shutdown :: Env -> IO ()
shutdown = hClose . envHandle

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
    h <- asks envHandle
    inChan <- asks envInChan
    readLoop h inChan
    where
        sendNickCommand config = write "NICK" [cfgNick config]
        sendUserCommand config = write "USER" [cfgNick config, "0", "*", "duck duck bot"]
        sendNickServPassword (Config { cfgNickServPassword=Just pw}) c = write "PRIVMSG" ["NickServ", "IDENTIFY " ++ pw] c
        sendNickServPassword (Config { cfgNickServPassword=Nothing}) _ = return ()
        sendJoinCommand config = write "JOIN" [cfgChannel config]
        write command params chan = liftIO $ writeChan chan $ OutIRCMessage (IRC.Message Nothing command params)

-- TODO: Use Data.ByteString here?

--
-- Reads data from the handle, converts them to IRC.Messages
-- and writes them to the envInChan for all command handlers
-- to consume them
--
readLoop :: Handle -> Chan InMessage -> EnvReader IO ()
readLoop h c = forever $ do
    s <- liftIO (hGetLine h)
    liftIO $ putStrLn $ "< " ++ s
    let d = IRC.decode s
    liftIO $ writeMsgToChan d c
    where
        writeMsgToChan (Just s) chan = writeChan chan (InIRCMessage s)
        writeMsgToChan Nothing _     = return ()

--
-- Reads Messages from the envOutChan and does whatever
-- is necessary to handle them, i.e. write IRC.Messages
-- to the handle or quit or ...
--
writeLoop :: Chan OutMessage -> Handle -> EnvReader IO ()
writeLoop c h = forever $ do
    msg <- liftIO $ readChan c
    case msg of
        OutIRCMessage m -> do
                              let s = IRC.encode m
                              liftIO $ hPutStr h (s ++ "\r\n")
                              liftIO $ putStrLn ("> " ++ s)

