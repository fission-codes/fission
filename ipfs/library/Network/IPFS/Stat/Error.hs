module Network.IPFS.Stat.Error (OverflowDetected (..)) where

import           Network.IPFS.Prelude

data OverflowDetected = OverflowDetected
  deriving (Eq, Show)

instance Display OverflowDetected where
  display OverflowDetected = "OverflowDetected"

instance ToJSON OverflowDetected where
  toJSON OverflowDetected = String "OverflowDetected"

instance FromJSON OverflowDetected where
  parseJSON =
    withScientific "OverflowDetected" \num ->
      if num < 0
        then return OverflowDetected
        else fail "Not an overflow"
