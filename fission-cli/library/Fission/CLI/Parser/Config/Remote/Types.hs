module Fission.CLI.Parser.Config.Remote.Types (RemoteConfig (..)) where

import           Fission.Prelude

import           Fission.User.DID.Types
import           Fission.Web.API.Remote

data RemoteConfig = RemoteConfig
  -- { target :: BaseUrl
  { remote :: Remote -- ^ Which remote peer / environment to use
  , mayDID :: Maybe DID -- ^ Cached remote DID
  } deriving (Show, Eq)
