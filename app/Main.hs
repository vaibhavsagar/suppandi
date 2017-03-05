{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Servant                  (serve)
import System.Environment       (getArgs)

import Server

dufferAPI :: Proxy API
dufferAPI = Proxy

main :: IO ()
main = do
    port:_ <- getArgs
    run (read port :: Int) (serve dufferAPI server)
