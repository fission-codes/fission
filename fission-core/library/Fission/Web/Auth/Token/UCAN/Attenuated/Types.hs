module Fission.Web.Auth.Token.UCAN.Attenuated.Types (Attenuated (..)) where

import           Fission.Prelude

data Attenuated subset
  = AllInScope
  | Subset subset
  deriving (Eq, Show)

instance Arbitrary subset => Arbitrary (Attenuated subset) where
  arbitrary =
    frequency
      [ (1, pure AllInScope)
      , (4, Subset <$> arbitrary)
      ]

instance ToJSON sub => ToJSON (Attenuated sub) where
  toJSON AllInScope         = String "*"
  toJSON (Subset rawSubset) = toJSON rawSubset

instance FromJSON sub => FromJSON (Attenuated sub) where
  parseJSON (String "*") = pure AllInScope
  parseJSON subset       = Subset <$> parseJSON subset
