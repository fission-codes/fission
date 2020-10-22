module Fission.WNFS.Privilege.Types (Privilege (..)) where

import           Fission.Prelude

import           Fission.WNFS.Capability.Types
import           Fission.WNFS.Subgraph.Types

data Privilege = Privilege
  { subgraph   :: !Subgraph
  , capability :: !Capability
  }
  deriving (Show, Eq)

instance Arbitrary Privilege where
  arbitrary = do
    subgraph   <- arbitrary
    capability <- arbitrary

    return Privilege {..}

instance ToJSON Privilege where
  toJSON Privilege {..} =
    object
      [ "wnfs" .= subgraph
      , "cap"  .= capability
      ]

instance FromJSON Privilege where
  parseJSON = withObject "WNFS.Privilege" \obj -> do
    subgraph   <- obj .: "wnfs"
    capability <- obj .: "cap"

    return Privilege {..}
