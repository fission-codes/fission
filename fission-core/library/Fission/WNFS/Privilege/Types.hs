module Fission.WNFS.Privilege.Types
  ( Privilege (..)
  -- * Reexports
  , module Fission.WNFS.Privilege.Capability.Types
  , module Fission.WNFS.Subgraph.Types
  ) where

import           Fission.Prelude

--import qualified Fission.Authorization.Allowable         as Allowable
-- FIXME delete PrivilegfeFor

import           Fission.WNFS.Privilege.Capability.Types
import           Fission.WNFS.Subgraph.Types

data Privilege = Privilege
  { subgraph   :: !Subgraph
  , capability :: !Capability
  }
  deriving (Show, Eq)

-- type instance Allowable.Privilege Subgraph = Privilege

instance Arbitrary Privilege where
  arbitrary = do
    subgraph   <- arbitrary
    capability <- arbitrary

    return Privilege {..}

instance PartialOrder Privilege where
  relationship x y =
    case (sgRel, capRel) of
      (Equal,      Equal)      -> Equal

      (Descendant, Descendant) -> Descendant
      (Descendant, Equal)      -> Descendant
      (Equal,      Descendant) -> Descendant

      (Ancestor,   Ancestor)   -> Ancestor
      (Ancestor,   Equal)      -> Ancestor
      (Equal,      Ancestor)   -> Ancestor

      (_,          _)          -> Sibling

    where
      sgRel  = relationship (subgraph   x) (subgraph   y)
      capRel = relationship (capability x) (capability y)

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
