module Network.IPFS.Stat.Types
  ( Stat (..)
  , module Network.IPFS.Stat.Error
  ) where

import qualified Data.Aeson.Types as JSON
import           Data.Scientific

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
    blockSize      <- detectOverflow =<< obj .: "BlockSize"
    cumulativeSize <- detectOverflow =<< obj .: "CumulativeSize"
    dataSize       <- detectOverflow =<< obj .: "DataSize"

    hash           <- obj .: "Hash"
    linksSize      <- obj .: "LinksSize"
    numLinks       <- obj .: "NumLinks"

    return Stat {..}

detectOverflow :: Scientific -> JSON.Parser (Either OverflowDetected Bytes)
detectOverflow num = checkOverflow <|> checkBytes
  where
    checkOverflow = Left  <$> parseJSON (JSON.Number num)
    checkBytes    = Right <$> parseJSON (JSON.Number num)
