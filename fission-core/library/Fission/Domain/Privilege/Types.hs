module Fission.Domain.Privilege.Types
  ( Privilege (..)
  , module Fission.Domain.Privilege.Capability.Types
  ) where

import qualified RIO.Text                                  as Text

import           Fission.Prelude

import           Fission.Domain.Privilege.Capability.Types
import           Fission.URL.DomainName.Types

data Privilege = Privilege
  { domain     :: !DomainName
  , capability :: !Capability
  }
  deriving (Show, Eq)

instance Display Privilege where
  textDisplay = Text.pack . show

instance PartialOrder Privileg where
  relationship domA domB =
    case (domRel, capRel) of
      (Equal, Equal) -> Equal
      (Equal, rel)   -> rel
      _              -> Siblings

    where
      domainRel = relationship (domain     domA) (domain     domB)
      capRel    = relationship (capability domA) (capability domB)

instance Arbitrary Privilege where
  arbitrary = do
    domain     <- arbitrary
    capability <- arbitrary

    return Privilege {..}

instance ToJSON Privilege where
  toJSON Privilege {..} =
    object
      [ "domain" .= domain
      , "cap"    .= capability
      ]

instance FromJSON Privilege where
  parseJSON = withObject "Domain.Privilege" \obj -> do
    domain     <- obj .: "domain"
    capability <- obj .: "cap"

    return Privilege {..}
