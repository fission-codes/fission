module Network.IPFS.Stat.Types
  ( Stat (..)
  , module Network.IPFS.Stat.Error
  ) where

import           Network.IPFS.Bytes.Types
import           Network.IPFS.Stat.Error

import           Network.IPFS.Prelude

data Stat = Stat
  { blockSize      :: Either OverflowDetected Bytes
  , cumulativeSize :: Either OverflowDetected Bytes
  , dataSize       :: Either OverflowDetected Bytes
  , hash           :: Text
  , linksSize      :: Bytes
  , numLinks       :: Natural
  }

instance FromJSON Stat where
  parseJSON = withObject "Stat" \obj -> do
    blockSize      <- obj .: "BlockSize"
    cumulativeSize <- obj .: "CumulativeSize"
    dataSize       <- obj .: "DataSize"

    hash           <- obj .: "Hash"
    linksSize      <- obj .: "LinksSize"
    numLinks       <- obj .: "NumLinks"

    return Stat {..}

