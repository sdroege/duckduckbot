module DuckDuckBot.Types (
    Env (..),
    EnvReader,
    Config (..),
    InMessage (..),
    OutMessage (..),
    MessageHandler,
    MessageHandlerEnv (..),
    MessageHandlerEnvReader,
    module DuckDuckBot.Connection
) where

import DuckDuckBot.Connection

import Control.Monad.Reader

import qualified Network.IRC as IRC
import Control.Concurrent.Chan

type EnvReader = ReaderT Env
data Env = Env {
    envConfig      :: Config,
    envConnection  :: Connection,
    envInChan      :: Chan InMessage, -- This is duplicated to all message handlers
    envOutChan     :: Chan OutMessage
}

data Config = Config {
    cfgServer           :: String,
    cfgPort             :: Int,
    cfgNick             :: String,
    cfgNickServPassword :: Maybe String,
    cfgChannel          :: String,
    cfgUseSsl           :: Bool
} deriving (Show)

type MessageHandlerEnvReader = ReaderT MessageHandlerEnv
data MessageHandlerEnv = MessageHandlerEnv {
    messageHandlerEnvNick    :: String,
    messageHandlerEnvChannel :: String
}

data InMessage  = InIRCMessage IRC.Message |
                  Quit

data OutMessage = OutIRCMessage IRC.Message

type MessageHandler = Chan InMessage -> Chan OutMessage -> MessageHandlerEnvReader IO ()

