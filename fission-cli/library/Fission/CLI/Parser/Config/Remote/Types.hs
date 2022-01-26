module Fission.CLI.Parser.Config.Remote.Types (RemoteConfig (..)) where

import           Fission.Prelude

import           Fission.Web.API.Remote
import           Web.DID.Types

data RemoteConfig = RemoteConfig
  { remote :: Remote -- ^ Which remote peer / environment to use
  , mayDID :: Maybe DID -- ^ Cached remote DID
  } deriving (Show, Eq)
