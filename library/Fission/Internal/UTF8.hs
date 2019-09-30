module Fission.Internal.UTF8
  ( Textable (..)
  , putText
  , showLazyBS
  , stripN
  , stripNBS
  , stripNewline
  , textToLazyBS
  , textShow
  ) where

import           RIO
import qualified RIO.ByteString      as Strict
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

class Textable a where
  encode :: a -> Either UnicodeException Text

instance Textable ByteString where
  encode = decodeUtf8'

instance Textable Lazy.ByteString where
  encode = encode . Lazy.toStrict

showLazyBS :: Show a => a -> Lazy.ByteString
showLazyBS = textToLazyBS . textDisplay . displayShow

textToLazyBS :: Text -> Lazy.ByteString
textToLazyBS = Lazy.fromStrict . Text.encodeUtf8

stripNewline :: Lazy.ByteString -> Lazy.ByteString
stripNewline bs = fromMaybe bs $ Lazy.stripSuffix "\n" bs

textShow :: Show a => a -> Text
textShow = textDisplay . displayShow

stripNBS :: Natural -> Lazy.ByteString -> Lazy.ByteString
stripNBS n bs = Lazy.drop i $ Lazy.take ((Lazy.length bs) - i) bs
  where
    i :: Int64
    i = fromIntegral n

stripN :: Natural -> Text -> Text
stripN n = Text.dropEnd i . Text.drop i
  where
    i :: Int
    i = fromIntegral n

-- | Helper for printing 'Text' to a console
putText :: MonadIO m => Text -> m ()
putText = Strict.putStr . encodeUtf8
