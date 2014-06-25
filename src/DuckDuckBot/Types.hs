module DuckDuckBot.Types (
    Env (..),
    EnvReader,
    Config (..),
    InMessage (..),
    OutMessage (..),
    MessageHandler,
    MessageHandlerEnv (..),
    MessageHandlerEnvReader,
    MessageHandlerMetadata (..),
    module DuckDuckBot.Connection
) where

import DuckDuckBot.Connection

import Control.Monad.Reader

import qualified Network.IRC as IRC
import Control.Concurrent.Chan
import Control.Concurrent.MVar

type EnvReader = ReaderT Env
data Env = Env {
    envConfig      :: Config,
    envConnection  :: Connection,
    envInChan      :: Chan InMessage, -- This is duplicated to all message handlers
    envOutChan     :: Chan OutMessage,
    envAuthUser    :: MVar (Maybe IRC.Prefix)
}

data Config = Config {
    cfgServer           :: String,
    cfgPort             :: Int,
    cfgNick             :: String,
    cfgNickServPassword :: Maybe String,
    cfgChannel          :: String,
    cfgUseSsl           :: Bool,
    cfgAuthPassword     :: Maybe String
} deriving (Show)

type MessageHandlerEnvReader = ReaderT MessageHandlerEnv
data MessageHandlerEnv = MessageHandlerEnv {
    messageHandlerEnvNick    :: String,
    messageHandlerEnvChannel :: String,
    messageHandlerEnvIsAuthUser :: IRC.Prefix -> IO Bool
}

-- More messages to be added here if we ever have
-- to tell our command handlers to do anything
data InMessage  = InIRCMessage IRC.Message | Quit
data OutMessage = OutIRCMessage IRC.Message

type MessageHandler = Chan InMessage -> Chan OutMessage -> MessageHandlerEnvReader IO ()

data MessageHandlerMetadata = MessageHandlerMetadata {
    messageHandlerMetadataName :: String,
    messageHandlerMetadataCommands :: [String],
    messageHandlerMetadataHandler :: MessageHandler
}
