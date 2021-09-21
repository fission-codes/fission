module Fission.User.DID.ION.Document
  ( getValidationPKs
  , GetDocAPI
  , module Fission.User.DID.ION.Document.Types
  ) where

import qualified Network.HTTP.Client                 as HTTP
import           Servant.API
import           Servant.Client

import           Fission.Prelude

import           Fission.User.DID.ION.Document.Types
import           Fission.User.DID.Types

type GetDocAPI = "1.0" :> "identifiers" :> Capture "did" DID :> Get '[JSON] ValidPKs

getValidationPKs :: MonadIO m => HTTP.Manager -> DID -> m (Either ClientError ValidPKs)
getValidationPKs manager did = liftIO $ runClientM (req did) env
  where
    env = mkClientEnv manager ionBaseUrl

req :: DID -> ClientM ValidPKs
req = client $ Proxy @GetDocAPI

ionBaseUrl :: BaseUrl
ionBaseUrl =  BaseUrl Https "beta.discover.did.microsoft.com" 443 ""
