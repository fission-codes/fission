module Fission.Web.Auth.Token.UCAN.Fact.Types (Fact (..)) where

import qualified RIO.Text        as Text

import           Fission.Prelude

data Fact
  = RawText Text
  deriving (Show, Eq)

instance Display Fact where
  textDisplay = Text.pack . show

instance ToJSON Fact where
  toJSON (RawText txt) = String txt

instance FromJSON Fact where
  parseJSON = withText "Fact" \txt -> pure $ RawText txt

instance Arbitrary Fact where
  arbitrary = RawText <$> arbitrary
