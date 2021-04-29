module Fission.CLI.Remote
  ( getRemoteURL
  , getRemoteBaseUrl
  , getNameService
  , getIpfsGateway
  -- * Reexports
  , module Fission.CLI.Remote.Class
  , module Fission.Web.API.Remote
  )  where

import           Servant.Client.Core

import           Fission.Prelude

import           Fission.URL.Types
import           Fission.Web.API.Remote

import           Fission.CLI.Remote.Class

getRemoteBaseUrl :: MonadRemote m => m BaseUrl
getRemoteBaseUrl = toBaseUrl <$> getRemote

getIpfsGateway :: MonadRemote m => m String
getIpfsGateway =
  getRemote >>= \case
    Production     -> fromBase production
    Staging        -> fromBase staging
    LocalDev       -> return . showBaseUrl $ BaseUrl Http "localhost" 5001 "ipns"
    Custom baseUrl -> fromBase baseUrl
  where
    fromBase url@BaseUrl{..} =
      return $ showBaseUrl url
        { baseUrlHost = "ipfs." <> baseUrlHost
        , baseUrlPath = "ipns"
        }

getRemoteURL :: MonadRemote m => m URL
getRemoteURL = toURL <$> getRemote

getNameService :: MonadRemote m => m URL
getNameService = toNameService <$> getRemote
