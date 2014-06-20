module DuckDuckBot.Config (
    getConfig
) where

import DuckDuckBot.Types (Config (..))

import Data.Maybe
import System.Environment

getConfig :: IO Config
getConfig = do
    server           <- lookupEnv "DDB_SERVER"
    port             <- lookupEnv "DDB_PORT"
    nick             <- lookupEnv "DDB_NICK"
    nickServPassword <- lookupEnv "DDB_NICKSERV_PASSWORD"
    channel          <- lookupEnv "DDB_CHANNEL"
    useSsl           <- lookupEnv "DDB_USE_SSL"
    return Config {
        cfgServer=fromJust server,
        cfgPort=read $ fromJust port,
        cfgNick=fromJust nick,
        cfgNickServPassword=nickServPassword,
        cfgChannel=fromJust channel,
        cfgUseSsl=fromJust useSsl == "1"
    }
-- FIXME: Error checking
