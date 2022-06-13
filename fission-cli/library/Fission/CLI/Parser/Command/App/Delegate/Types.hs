module Fission.CLI.Parser.Command.App.Delegate.Types (Options (..)) where

import           Fission.Prelude

import           Web.DID.Types

data Options = Options
  { appName :: Text
  , audienceDid :: Maybe DID
  } deriving (Show, Eq)
