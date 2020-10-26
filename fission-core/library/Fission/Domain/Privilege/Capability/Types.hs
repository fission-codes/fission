module Fission.Domain.Privilege.Capability.Types (Capability (..)) where

import qualified RIO.Text        as Text

import           Fission.Prelude

data Capability
  = SuperUser
  deriving (Show, Eq, Ord)
  deriving PartialOrder via TotalOrder Capability

instance Display Capability where
  display SuperUser = "Domain.SuperUser"

instance Arbitrary Capability where
  arbitrary = return SuperUser

instance ToJSON Capability where
  toJSON SuperUser = "SUPER_USER"

instance FromJSON Capability where
  parseJSON = withText "Domain.Capability" \txt ->
    case Text.toUpper txt of
      "SUPER_USER" -> pure SuperUser
      other        -> fail $ show other <> " is not a valid Web App capabilty"
