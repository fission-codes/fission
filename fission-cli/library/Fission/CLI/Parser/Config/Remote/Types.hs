module Fission.CLI.Parser.Config.Remote.Types (RemoteConfig (..)) where

import           Servant.Client

import           Fission.Prelude

import           Fission.User.DID.Types

data RemoteConfig = RemoteConfig
  { target :: !BaseUrl     -- ^ Which remote peer / server to use
  , mayDID :: !(Maybe DID) -- ^ Cached remote DID
  } deriving (Show, Eq)
