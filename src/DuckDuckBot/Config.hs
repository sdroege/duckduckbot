module DuckDuckBot.Config (
    newConfig,
    defaultConfig
) where

import DuckDuckBot.Types (Config (..))

import Data.Maybe
import System.Environment

defaultConfig :: IO Config
defaultConfig = return Config {
        cfgServer="irc.freenode.org",
        cfgPort=6667,
        cfgNick="duckduckbot",
        cfgNickServPassword=Just "ahquohph",
        cfgChannel="#blablablablabla"
    }

newConfig :: IO Config
newConfig = do
    server           <- lookupEnv "DDB_SERVER"
    port             <- lookupEnv "DDB_PORT"
    nick             <- lookupEnv "DDB_NICK"
    nickServPassword <- lookupEnv "DDB_NICKSERV_PASSWORD"
    channel          <- lookupEnv "DDB_CHANNEL"
    return Config {
        cfgServer=fromJust server,
        cfgPort=read $ fromJust port,
        cfgNick=fromJust nick,
        cfgNickServPassword=nickServPassword,
        cfgChannel=fromJust channel
    }
-- FIXME: Error checking
