module Fission.Web.Auth.Token.UCAN.Resource.Scope.Types (Scope (..)) where

import           Fission.Prelude

data Scope subset
  = Complete
  | Subset subset
  | None
  deriving (Eq, Show)

instance Arbitrary subset => Arbitrary (Scope subset) where
  arbitrary =
    frequency
      [ (1, pure Complete)
      , (1, pure None)
      , (4, Subset <$> arbitrary)
      ]

instance ToJSON sub => ToJSON (Scope sub) where
  toJSON Complete           = String "*"
  toJSON None               = Null
  toJSON (Subset rawSubset) = toJSON rawSubset

instance FromJSON sub => FromJSON (Scope sub) where
  parseJSON (String "*") = pure Complete
  parseJSON Null         = pure None
  parseJSON subset       = Subset <$> parseJSON subset
