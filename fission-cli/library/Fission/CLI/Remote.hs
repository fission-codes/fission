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
getIpfsGateway = do
  url <- getRemoteBaseUrl
  let BaseUrl {..} = url
      ipfsGateway = url { baseUrlHost = "ipfs." <> baseUrlHost, baseUrlPath = "ipns" }

  pure $ showBaseUrl ipfsGateway

getRemoteURL :: MonadRemote m => m URL
getRemoteURL = toURL <$> getRemote

getNameService :: MonadRemote m => m URL
getNameService = toNameService <$> getRemote
