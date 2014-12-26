{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    ) where

import DuckDuckBot.Config
import DuckDuckBot.Core
import DuckDuckBot.Types

import Control.Exception
import Control.Concurrent (threadDelay)
import System.Exit

main :: IO ()
main = do
    config <- getConfig
    runUntilQuit config
    exitSuccess

    where
        runUntilQuit config =
            catches (run config)
                [ Handler (\(e :: IOException)      -> retry e config)
                , Handler (\(e :: TimeoutException) -> retry e config)
                , Handler (\(e :: SomeException)    -> abort e)
                ]

        retry :: (Exception e) => e -> Config -> IO ()
        retry e config = do
            putStrLn $ "Exception while running, restarting: " ++ show e
            threadDelay 1000000
            runUntilQuit config

        abort e = do
            putStrLn $ "Exception while running: " ++ show e
            exitFailure

