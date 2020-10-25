module Fission.Domain.Privilege.Types
  ( Privilege (..)
  , module Fission.Domain.Privilege.Capability.Types
  ) where

import qualified RIO.Text                                  as Text

import           Fission.Prelude

import           Fission.Domain.Privilege.Capability.Types
import           Fission.URL.DomainName.Types

data Privilege = Privilege
  { domainName :: !DomainName
  , capability :: !Capability
  }
  deriving (Show, Eq)

instance Display Privilege where
  textDisplay = Text.pack . show

instance PartialOrder Privilege where
  relationship a b =
    case (domRel, capRel) of
      (Equal, rel) -> rel
      _            -> Sibling

    where
      capRel = relationship (capability a) (capability b)
      domRel = relationship (domainName a) (domainName b)

instance Arbitrary Privilege where
  arbitrary = do
    domainName <- arbitrary
    capability <- arbitrary

    return Privilege {..}

instance ToJSON Privilege where
  toJSON Privilege {..} =
    object
      [ "domain" .= domainName
      , "cap"    .= capability
      ]

instance FromJSON Privilege where
  parseJSON = withObject "Domain.Privilege" \obj -> do
    domainName <- obj .: "domain"
    capability <- obj .: "cap"

    return Privilege {..}
