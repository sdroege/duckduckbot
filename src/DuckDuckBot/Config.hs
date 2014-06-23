module DuckDuckBot.Config (
    getConfig
) where

import DuckDuckBot.Types (Config (..))

import Data.Maybe
import System.Environment
import Control.Monad

import Safe

#if !MIN_VERSION_base(4,6,0)
import Control.Exception
import System.IO.Error(isDoesNotExistError)

lookupEnv :: String -> IO (Maybe String)
lookupEnv k =
    let checkException e | isDoesNotExistError e = return Nothing
                         | otherwise             = throwIO e
    in handle checkException (getEnv k >>= return . Just)
#endif

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
    let port = readMay (fromJust portString) :: Maybe Int
    when (isNothing port) $ error "Need to set port in $DDB_PORT"
    when (isNothing nick) $ error "Need to set nickname in $DDB_NICK"
    when (isNothing channel) $ error "Need to set channel in $DDB_CHANNEL"

    return Config {
        cfgServer=fromJust server,
        cfgPort=fromJust port,
        cfgNick=fromJust nick,
        cfgNickServPassword=nickServPassword,
        cfgChannel=fromJust channel,
        cfgUseSsl=useSslString == Just "1"
    }
