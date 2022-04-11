module Web.UCAN.Nonce.Types
  ( Nonce(..)
  ) where

import           Control.Monad   (replicateM)
import           Data.Aeson
import           RIO
import qualified RIO.Text        as Text
import           Test.QuickCheck


newtype Nonce = Nonce { unNonce :: Text }
  deriving (Eq, Show, Ord)

instance Arbitrary Nonce where
  arbitrary = Nonce . Text.pack <$> replicateM 6 (elements chars)
    where
      chars =
        ['A'..'Z']
        <> ['a'..'z']
        <> ['0'..'9']

instance FromJSON Nonce where
  parseJSON = withText "UCAN Nonce" (pure . Nonce)

instance ToJSON Nonce where
  toJSON = String . unNonce
