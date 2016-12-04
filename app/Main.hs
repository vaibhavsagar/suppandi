{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text
import Data.Text.Encoding
import Network.Wai.Handler.Warp
import Servant

import Duffer.Unified
import Duffer.Loose.Objects (GitObject)
import Duffer.WithRepo

type API = "git" :> Capture    "ref"  Text :> Get '[JSON] GitObject
      :<|> "ref" :> CaptureAll "path" Text :> Get '[JSON] GitObject

server :: Server API
server = serveObject
    :<|> serveRef

serveObject :: Text -> Handler GitObject
serveObject textRef = liftIO (withRepo ".git" (readObject ref)) >>=
    maybe (throwError err404 {errBody = "Object not found."}) return
    where ref = encodeUtf8 textRef

serveRef :: [Text] -> Handler GitObject
serveRef textPath = liftIO (withRepo ".git" (resolveRef path)) >>=
    maybe (throwError err404 {errBody = "Reference not found."}) return
    where path = unpack $ intercalate "/" textPath

dufferAPI :: Proxy API
dufferAPI = Proxy

main :: IO ()
main = run 8081 (serve dufferAPI server)
