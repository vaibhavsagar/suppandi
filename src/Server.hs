{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (API, server) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson

import Data.Text              (Text, intercalate, unpack)
import Data.Text.Encoding     (encodeUtf8, decodeUtf8)
import Servant

import Duffer.Unified       (readObject, resolveRef, writeObject)
import Duffer.Loose.Objects (GitObject, Ref)
import Duffer.WithRepo      (WithRepo, liftIO, withRepo)

newtype JSONRef = JSONRef Ref

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

execute :: (MonadIO m) => FilePath -> WithRepo a -> m a
execute path = liftIO . withRepo path

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
