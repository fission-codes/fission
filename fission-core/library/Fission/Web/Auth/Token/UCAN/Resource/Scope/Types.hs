module Fission.Web.Auth.Token.UCAN.Resource.Scope.Types (Scope (..)) where

import           Fission.Prelude

data Scope subset
  = Complete
  | Subset subset
  | None
  deriving (Eq, Ord, Show)

instance Arbitrary subset => Arbitrary (Scope subset) where
  arbitrary =
    frequency
      [ (1, pure Complete)
      , (4, Subset <$> arbitrary)
      , (1, pure None)
      ]

instance ToJSON sub => ToJSON (Scope sub) where
  toJSON Complete           = String "*"
  toJSON (Subset rawSubset) = toJSON rawSubset
  toJSON None               = Null

instance FromJSON sub => FromJSON (Scope sub) where
  parseJSON (String "*") = pure Complete
  parseJSON Null         = pure None
  parseJSON subset       = Subset <$> parseJSON subset

instance ToJSON a => ToJSONKey (Scope a) where
  toJSONKey = ToJSONKeyValue f g
    where
      f = \case
        Complete -> String "*"
        Subset a -> toJSON a
        None     -> Null

      g = \case
        Complete -> toEncoding $ String "*"
        Subset a -> toEncoding a
        None     -> toEncoding Null

instance FromJSON a => FromJSONKey (Scope a) where
  fromJSONKey = FromJSONKeyValue \case
    String "*" -> return Complete
    val        -> parseJSON val
