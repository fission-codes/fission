module Fission.CLI.Parser.Config.Remote.Types (RemoteConfig (..)) where

-- import           Servant.Client

import           Fission.Prelude

import           Fission.User.DID.Types

import           Fission.CLI.Remote.Types

data RemoteConfig = RemoteConfig
  -- { target :: BaseUrl
  { remote :: Remote -- ^ Which remote peer / environment to use
  , mayDID :: Maybe DID -- ^ Cached remote DID
  } deriving (Show, Eq)
