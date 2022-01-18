module Fission.Web.Auth.Token.UCAN.Resource.Scope.Types (Scope (..)) where

import           Fission.Prelude

import           Web.JWT.Proof.Class


data Scope subset
  = Complete
  | Subset subset
  deriving (Eq, Ord, Show)

instance ResourceSemantics rsc => ResourceSemantics (Scope rsc) where
  Complete          `canDelegate` _            = True
  (Subset _)        `canDelegate` Complete     = False
  (Subset rscProof) `canDelegate` (Subset rsc) = rscProof `canDelegate` rsc

instance Arbitrary subset => Arbitrary (Scope subset) where
  arbitrary =
    frequency
      [ (1, pure Complete)
      , (4, Subset <$> arbitrary)
      ]

instance ToJSON sub => ToJSON (Scope sub) where
  toJSON Complete           = String "*"
  toJSON (Subset rawSubset) = toJSON rawSubset

instance FromJSON sub => FromJSON (Scope sub) where
  parseJSON (String "*") = pure Complete
  parseJSON subset       = Subset <$> parseJSON subset

instance ToJSON a => ToJSONKey (Scope a) where
  toJSONKey = ToJSONKeyValue f g
    where
      f = \case
        Complete -> String "*"
        Subset a -> toJSON a

      g = \case
        Complete -> toEncoding $ String "*"
        Subset a -> toEncoding a

instance FromJSON a => FromJSONKey (Scope a) where
  fromJSONKey = FromJSONKeyValue \case
    String "*" -> return Complete
    val        -> parseJSON val
