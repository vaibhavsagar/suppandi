{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Data.Text
import Data.Text.Encoding
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

import Duffer.Unified
import Duffer.Loose.Objects (GitObject)
import Duffer.WithRepo

type API = "git" :> Capture "ref" Text :> Get '[JSON] GitObject

server :: Server API
server = serveObject

serveObject :: Text -> Handler GitObject
serveObject textRef = do
    let ref = encodeUtf8 textRef
    maybeObject <- liftIO $ withRepo ".git" (readObject ref)
    maybe
        (throwError err404 {errBody = "Object not found."})
        return
        maybeObject

userAPI :: Proxy API
userAPI = Proxy

main :: IO ()
main = run 8081 (serve userAPI server)
