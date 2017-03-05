{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (API, server) where

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
import Duffer.Unified
import Duffer.Loose.Objects (GitObject, Ref)
import Duffer.WithRepo

newtype JSONRef = JSONRef { unJSONRef :: Ref }

instance ToJSON JSONRef where
    toJSON     (JSONRef ref) = object ["ref" .= decodeUtf8 ref]
    toEncoding (JSONRef ref) = pairs  ("ref" .= decodeUtf8 ref)

instance FromJSON JSONRef where
    parseJSON (Object v) = JSONRef <$> (encodeUtf8 <$> v .: "ref")

type API
    =    "git" :> Capture    "ref"   Text      :> Get  '[JSON] GitObject
    :<|> "ref" :> CaptureAll "path"  Text      :> Get  '[JSON] GitObject
    :<|> "put" :> ReqBody    '[JSON] GitObject :> Post '[JSON] JSONRef

server :: Server API
server = serveObject
    :<|> serveRef
    :<|> writeObj

serveObject :: Text -> Handler GitObject
serveObject textRef = liftIO (withRepo ".git" (readObject ref)) >>=
    maybe (throwError err404 {errBody = "Object not found."}) return
    where ref = encodeUtf8 textRef

serveRef :: [Text] -> Handler GitObject
serveRef textPath = liftIO (withRepo ".git" (resolveRef path)) >>=
    maybe (throwError err404 {errBody = "Reference not found."}) return
    where path = unpack $ intercalate "/" textPath

writeObj :: GitObject -> Handler JSONRef
writeObj obj = JSONRef <$> liftIO (withRepo ".git" (writeObject obj))
