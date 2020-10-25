module Fission.WNFS.Privilege.Types
  ( Privilege (..)
  -- * Reexports
  , module Fission.WNFS.Privilege.Capability.Types
  , module Fission.WNFS.Subgraph.Types
  ) where

import           Fission.Prelude

import           Fission.Authorization.PrivilegeFor.Types

import           Fission.WNFS.Privilege.Capability.Types
import           Fission.WNFS.Subgraph.Types

data Privilege = Privilege
  { subgraph   :: !Subgraph
  , capability :: !Capability
  }
  deriving (Show, Eq)

type instance PrivilegeFor Subgraph = Privilege

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
