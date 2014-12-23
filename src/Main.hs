module Main
    ( main
    ) where

import DuckDuckBot.Config
import DuckDuckBot.Core
import Control.Exception
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    config <- getConfig
    runUntilQuit config

    where
        runUntilQuit config = do
            catch (run config)
            $ \(SomeException e) -> do
                print $ "Exception while running, restarting: " ++ show e
                threadDelay 1000000
                runUntilQuit config

