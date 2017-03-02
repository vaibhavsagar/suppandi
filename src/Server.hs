{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

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

type UserAPI1 = "users" :> Get '[JSON] [User]

server1 :: Server UserAPI1
server1 = return users1

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1
