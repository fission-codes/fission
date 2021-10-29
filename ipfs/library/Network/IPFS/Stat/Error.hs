module Network.IPFS.Stat.Error (OverflowDetected (..)) where

import qualified RIO.Text             as Text

import           Network.IPFS.Prelude

data OverflowDetected = OverflowDetected
  deriving (Eq, Show)

instance Display OverflowDetected where
  display OverflowDetected = "OverflowDetected"

instance ToJSON OverflowDetected where
  toJSON OverflowDetected = String "OverflowDetected"

instance FromJSON OverflowDetected where
  parseJSON =
    withText "OverflowDetected" \txt ->
      if "-" `Text.isPrefixOf` txt
        then return OverflowDetected
        else fail "Not an overflow"
