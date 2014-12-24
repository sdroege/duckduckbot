{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    ) where

import DuckDuckBot.Config
import DuckDuckBot.Core
import DuckDuckBot.Types

import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async

main :: IO ()
main = do
    config <- getConfig
    runUntilQuit config

    where
        runUntilQuit config =
            catches (async (run config) >>= wait)
                [ Handler (\(e :: IOException)      -> retry e config)
                , Handler (\(e :: TimeoutException) -> retry e config)
                , Handler (\(e :: SomeException)    -> abort e)
                ]

        retry :: (Exception e) => e -> Config -> IO ()
        retry e config = do
            putStrLn $ "Exception while running, restarting: " ++ show e
            threadDelay 1000000
            runUntilQuit config

        abort e = putStrLn $ "Exception while running: " ++ show e

