module Fission.App.Privilege.Capability.Types (Capability (..)) where

import qualified RIO.Text        as Text

import           Fission.Prelude

data Capability
  = Revise
  | SuperUser
  deriving (Show, Eq, Ord)
  deriving PartialOrder via TotalOrder Capability

instance Display Capability where
  display Revise    = "App.Revise"
  display SuperUser = "App.SuperUser"

instance Arbitrary Capability where
  arbitrary = elements
    [ Revise
    , SuperUser
    ]

instance ToJSON Capability where
  toJSON = \case
    Revise     -> "REVISE"
    SuperUser  -> "SUPER_USER"

instance FromJSON Capability where
  parseJSON = withText "App.Capability" \txt ->
    case Text.toUpper txt of
      "REVISE"     -> pure Revise
      "SUPER_USER" -> pure SuperUser
      other        -> fail $ show other <> " is not a valid Web App capabilty"
