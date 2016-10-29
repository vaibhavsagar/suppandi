{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Text
import Data.Text.Encoding
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

import Duffer.Unified
import Duffer.Loose.Objects (GitObject)
import Duffer.Loose.JSON
import Duffer.WithRepo

type API = "git" :> Capture "ref" Text :> Get '[JSON] GitObject

server :: Server API
server = serveObject

serveObject :: Text -> Handler GitObject
serveObject textRef = do
    let ref = encodeUtf8 textRef
    maybeObject <- liftIO $ withRepo ".git" (readObject ref)
    maybe
        (throwError err404 {errBody = "git object not found"})
        return
        maybeObject

userAPI :: Proxy API
userAPI = Proxy

main :: IO ()
main = run 8081 (serve userAPI server)
