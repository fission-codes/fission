module Fission.WNFS.Privilege.Capability.Types (Capability (..)) where

import qualified RIO.Text        as Text

import           Fission.Prelude

data Capability
  = Create
  | Revise
  | SoftDelete
  | Overwrite
  | SuperUser
  deriving (Show, Eq, Ord)
  deriving PartialOrder via TotalOrder Capability

instance Arbitrary Capability where
  arbitrary = elements
    [ Create
    , Revise
    , SoftDelete
    , Overwrite
    , SuperUser
    ]

instance ToJSON Capability where
  toJSON = \case
    Create     -> "CREATE"
    Revise     -> "REVISE"
    SoftDelete -> "SOFT_DELETE"
    Overwrite  -> "OVERWRITE"
    SuperUser  -> "SUPER_USER"

instance FromJSON Capability where
  parseJSON = withText "WNFS.Capability" \txt ->
    case Text.toUpper txt of
      "CREATE"      -> pure Create
      "REVISE"      -> pure Revise
      "SOFT_DELETE" -> pure SoftDelete
      "OVERWRITE"   -> pure Overwrite
      "SUPER_USER"  -> pure SuperUser
      other         -> fail $ show other <> " is not a valid WNFS capabilty"
