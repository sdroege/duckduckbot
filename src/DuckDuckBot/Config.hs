module DuckDuckBot.Config (
    getConfig
) where

import DuckDuckBot.Types (Config (..))

import Data.Maybe
import System.Environment
import Control.Monad
import Control.Exception

getConfig :: IO Config
getConfig = do
    server           <- lookupEnv "DDB_SERVER"
    portString       <- lookupEnv "DDB_PORT"
    nick             <- lookupEnv "DDB_NICK"
    nickServPassword <- lookupEnv "DDB_NICKSERV_PASSWORD"
    channel          <- lookupEnv "DDB_CHANNEL"
    useSslString     <- lookupEnv "DDB_USE_SSL"

    when (isNothing server) $ error "Need to set server in $DDB_SERVER"
    when (isNothing portString) $ error "Need to set port in $DDB_PORT"
    port <- handle (\(SomeException _) -> error "Need to set port in $DDB_PORT") $ evaluate (read (fromJust portString) :: Int)
    when (isNothing nick) $ error "Need to set nickname in $DDB_NICK"
    when (isNothing channel) $ error "Need to set channel in $DDB_CHANNEL"

    return Config {
        cfgServer=fromJust server,
        cfgPort=port,
        cfgNick=fromJust nick,
        cfgNickServPassword=nickServPassword,
        cfgChannel=fromJust channel,
        cfgUseSsl=useSslString == Just "1"
    }
