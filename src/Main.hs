module Main
    ( main
    ) where

import DuckDuckBot.Config
import DuckDuckBot.Core

main :: IO ()
main = do
    config <- getConfig
    run config

