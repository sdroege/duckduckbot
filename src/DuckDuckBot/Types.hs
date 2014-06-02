module DuckDuckBot.Types (
    Env (..),
    EnvReader,
    Config (..),
    InMessage (..),
    OutMessage (..),
    MessageHandler,
    PluginEnv (..),
    PluginEnvReader
) where

import Control.Monad.Reader

import System.IO
import qualified Network.IRC as IRC
import Control.Concurrent.Chan

type EnvReader = ReaderT Env
data Env = Env {
    envConfig  :: Config,
    envHandle  :: Handle,
    envInChan  :: Chan InMessage, -- This is duplicated to all command handlers
    envOutChan :: Chan OutMessage
}

data Config = Config {
    cfgServer           :: String,
    cfgPort             :: Int,
    cfgNick             :: String,
    cfgNickServPassword :: Maybe String,
    cfgChannel          :: String
} deriving (Show)

type PluginEnvReader = ReaderT PluginEnv
data PluginEnv = PluginEnv {
    pluginEnvNick :: String
}

data InMessage  = InIRCMessage IRC.Message |
                  Quit

data OutMessage = OutIRCMessage IRC.Message

type MessageHandler = Chan InMessage -> Chan OutMessage -> PluginEnvReader IO ()

