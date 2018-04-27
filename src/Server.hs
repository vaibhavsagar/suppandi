{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Server (service) where

import Data.ByteString.Lazy (ByteString)
import Data.Coerce          (coerce)
import Data.Proxy           (Proxy(Proxy))
import Data.Text            (Text, intercalate, unpack)
import Data.Text.Encoding   (encodeUtf8)
import Duffer.Loose.Objects (GitObject)
import Duffer.Unified       (readObject, resolveRef, writeObject)
import Duffer.WithRepo      (WithRepo, liftIO, withRepo)
import Duffer.JSON          (GitObjectJSON(GitObjectJSON), RefJSON(RefJSON))
import Servant

type API = Capture "path" FilePath
    :> ("git"
            :>   (Capture "ref"   Text         :> Get  '[JSON] GitObjectJSON
            :<|> ReqBody '[JSON] GitObjectJSON :> Post '[JSON] RefJSON)
    :<|> "ref" :> CaptureAll "path" Text       :> Get  '[JSON] GitObjectJSON)

service :: Application
service = serve @API Proxy server

server :: Server API
server path = (serveObj path :<|> writeObj path) :<|> serveRef path

handle :: ByteString -> Maybe GitObject -> Handler GitObjectJSON
handle errMsg = maybe (throwError err404 {errBody = errMsg}) (return . coerce)

doer :: ByteString -> (a -> WithRepo (Maybe GitObject)) -> FilePath -> (a -> Handler GitObjectJSON)
doer err f path = (handle err =<<) . liftIO . withRepo path . f

serveObj :: FilePath -> Text -> Handler GitObjectJSON
serveObj = doer "Object not found." (readObject . encodeUtf8)

serveRef :: FilePath -> [Text] -> Handler GitObjectJSON
serveRef = doer "Reference not found." (resolveRef . unpack . intercalate "/")

writeObj :: FilePath -> GitObjectJSON -> Handler RefJSON
writeObj path = fmap coerce . liftIO . withRepo path . writeObject . coerce
