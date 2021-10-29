-- | UTF8 text helpers
module Network.IPFS.Internal.UTF8
  ( Textable (..)
  , stripN
  , textToLazyBS
  , textShow
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

class Textable a where
  encode :: a -> Either UnicodeException Text

instance Textable ByteString where
  encode = decodeUtf8'

instance Textable Lazy.ByteString where
  encode = encode . Lazy.toStrict

textToLazyBS :: Text -> Lazy.ByteString
textToLazyBS = Lazy.fromStrict . Text.encodeUtf8

textShow :: Show a => a -> Text
textShow = textDisplay . displayShow

stripN :: Natural -> Text -> Text
stripN n = Text.dropEnd i . Text.drop i
  where
    i :: Int
    i = fromIntegral n
