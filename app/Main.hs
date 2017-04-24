{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad            ((<=<))
import Data.Maybe               (listToMaybe, fromMaybe)
import Network.Wai.Handler.Warp (run)
import System.Environment       (getArgs)
import Text.Read                (readMaybe)

import Server (service)


main :: IO ()
main = do
    port <- fromMaybe 8080 . (readMaybe @Int <=< listToMaybe) <$> getArgs
    run port service
