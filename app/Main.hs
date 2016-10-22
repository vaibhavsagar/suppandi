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

data User = User
   { name              :: String
   , age               :: Int
   , email             :: String
   , registration_date :: Day
   } deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
    [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683 3  1)
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]

isaac, albert :: User
isaac  = User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683 3  1)
albert = User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

type UserAPI1 = "users" :> Get '[JSON] [User]

type UserAPI2
    =    "users"  :> Get '[JSON] [User]
    :<|> "albert" :> Get '[JSON]  User
    :<|> "isaac"  :> Get '[JSON]  User

server1 :: Server UserAPI1
server1 = return users1

server2 :: Server UserAPI2
server2
    =    return users2
    :<|> return albert
    :<|> return isaac

userAPI :: Proxy UserAPI2
userAPI = Proxy

app2 :: Application
app2 = serve userAPI server2


main :: IO ()
main = run 8081 app2
