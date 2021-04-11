module Fission.CLI.Parser.Command.User.Types (Options (..)) where

import           Fission.Prelude

import qualified Fission.CLI.Parser.Command.User.Login  as Login
import qualified Fission.CLI.Parser.Command.User.WhoAmI as WhoAmI

data Options
  = Login    Login.Options    -- ^ Log in to existing account
  | WhoAmI   WhoAmI.Options   -- ^ Show current logged in user
  deriving (Show, Eq)
