module Fission.App.Privilege.Types
  ( Privilege (..)
  , module Fission.App.Privilege.Capability.Types
  ) where

import qualified RIO.Text                               as Text

import           Fission.Prelude

import           Fission.App.Privilege.Capability.Types
import           Fission.URL.Types

data Privilege = Privilege
  { url        :: !URL
  , capability :: !Capability
  }
  deriving (Show, Eq)

instance Display Privilege where
  textDisplay = Text.pack . show

instance PartialOrder Privilege where
  relationship a b =
    case (urlRel, capRel) of
      (Equal,      Equal)      -> Equal

      (Descendant, Equal)      -> Descendant
      (Descendant, Descendant) -> Descendant

      (Ancestor,   Equal)      -> Ancestor
      (Ancestor,   Ancestor)   -> Ancestor

      _                        -> Sibling

    where
      urlRel = relationship (url a)        (url b)
      capRel = relationship (capability a) (capability b)

instance Arbitrary Privilege where
  arbitrary = do
    url        <- arbitrary
    capability <- arbitrary

    return Privilege {..}

instance ToJSON Privilege where
  toJSON Privilege {..} =
    object
      [ "web" .= url
      , "cap" .= capability
      ]

instance FromJSON Privilege where
  parseJSON = withObject "App.Privilege" \obj -> do
    url        <- obj .: "web"
    capability <- obj .: "cap"

    return Privilege {..}


