module Fission.CLI.Remote
  ( getRemoteURL
  , getRemoteBaseUrl
  , getNameService
  , BaseUrl(..)
  , Scheme(..)
  -- * Reexports
  , module Fission.CLI.Remote.Class
  , module Fission.Web.API.Remote
  )  where

import           Servant.Client.Core      (BaseUrl (..), Scheme (..))

import           Fission.Prelude

import           Fission.URL.Types
import           Fission.Web.API.Remote

import           Fission.CLI.Remote.Class

getRemoteBaseUrl :: MonadRemote m => m BaseUrl
getRemoteBaseUrl = toBaseUrl <$> getRemote

getRemoteURL :: MonadRemote m => m URL
getRemoteURL = toURL <$> getRemote

getNameService :: MonadRemote m => m URL
getNameService = toNameService <$> getRemote
