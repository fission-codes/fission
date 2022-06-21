module Fission.CLI.Parser.Command.Generate.Types (Options (..)) where

import           Fission.Prelude

import qualified Fission.CLI.Parser.Command.Generate.Credentials as Credentials

data Options
  = Credentials Credentials.Options    -- ^ Generate a key pair and DID
  deriving (Show, Eq)