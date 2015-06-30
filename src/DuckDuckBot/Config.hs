{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DuckDuckBot.Config (
    getConfig
) where

import DuckDuckBot.Types (Config (..))
import DuckDuckBot.Compat

import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Except

import Safe
import System.Environment (lookupEnv)

newtype Env a = Env { unEnv :: ExceptT String IO a }
    deriving (Functor, Applicative, Alternative)

getEnv :: Env a -> IO (Either String a)
getEnv = runExceptT . unEnv

env :: String -> Env String
env key = Env (liftIO (lookupEnv key) >>= maybe (throwError $ "Can't get environment variable " ++ key) return)

envRead :: Read a => String -> Env a
envRead key = Env (unEnv (env key) >>= maybe (throwError $ "Can't parse environment variable " ++ key) return . readMay)

config :: Env Config
config = Config
   <$> env "DDB_SERVER"
   <*> envRead "DDB_PORT"
   <*> env "DDB_NICK"
   <*> optional (env "DDB_NICKSERV_PASSWORD")
   <*> env "DDB_CHANNEL"
   <*> ((== "1") <$> env "DDB_USE_SSL")
   <*> optional (env "DDB_AUTH_PASSWORD")

getConfig :: IO Config
getConfig = do
    c <- getEnv config
    case c of
        Left  s  -> error ("Invalid config: " ++ s)
        Right c' -> return c'

