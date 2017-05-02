{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Server (service) where

import Data.Aeson           (ToJSON(..), FromJSON(..), Value(..), object, pairs
                            ,(.=), (.:))
import Data.Coerce          (coerce)
import Data.Proxy           (Proxy(..))
import Data.Text            (Text, intercalate, unpack)
import Data.Text.Encoding   (encodeUtf8, decodeUtf8)
import Duffer.Unified       (readObject, resolveRef, writeObject)
import Duffer.Loose.Objects (Ref)
import Duffer.WithRepo      (WithRepo, liftIO, withRepo)
import Duffer.JSON          (GitObjectJSON(GitObjectJSON))
import Servant

newtype JSONRef = JSONRef Ref

instance ToJSON JSONRef where
    toJSON     (JSONRef ref) = object ["ref" .= decodeUtf8 ref]
    toEncoding (JSONRef ref) = pairs  ("ref" .= decodeUtf8 ref)

instance FromJSON JSONRef where
    parseJSON (Object v) = coerce <$> (encodeUtf8 <$> v .: "ref")

type API = Capture "path" FilePath :>
    (    "git" :>
        (    Capture "ref"   Text          :> Get  '[JSON] GitObjectJSON
        :<|> ReqBody '[JSON] GitObjectJSON :> Post '[JSON] JSONRef
        )
    :<|> "ref" :> CaptureAll "path" Text :> Get '[JSON] GitObjectJSON
    )

service :: Application
service = serve (Proxy @API) server

server :: Server API
server path = (serveObj path :<|> writeObj path) :<|> serveRef path

execute :: FilePath -> WithRepo a -> Handler a
execute = ((.).(.)) liftIO withRepo

serveObj :: FilePath -> Text -> Handler GitObjectJSON
serveObj path textRef = execute path (readObject $ encodeUtf8 textRef) >>=
    maybe
        (throwError err404 {errBody = "Object not found."})
        (return . coerce)

serveRef :: FilePath -> [Text] -> Handler GitObjectJSON
serveRef repoPath textPath = execute repoPath (resolveRef path) >>=
    maybe
        (throwError err404 {errBody = "Reference not found."})
        (return . coerce)
    where path = unpack $ intercalate "/" textPath

writeObj :: FilePath -> GitObjectJSON -> Handler JSONRef
writeObj path obj = JSONRef <$> execute path (writeObject (coerce obj))
