module Fission.CLI.Parser.Command.App.Delegate.Types (Options (..)) where

import           Fission.Prelude

import           Fission.CLI.Parser.Quiet.Types

import           Fission.Web.Auth.Token.UCAN.Potency.Types

import           Web.DID.Types

data Options = Options
  { appName :: Text
  , potency :: Either String Potency
  , audienceDid :: Either String DID
  , lifetimeInSeconds :: Int
  , quiet :: QuietFlag
  } deriving (Show, Eq)
