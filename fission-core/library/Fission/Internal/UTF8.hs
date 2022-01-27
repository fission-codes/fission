-- | UTF8 text helpers
module Fission.Internal.UTF8
  ( Textable (..)
  , putText
  , putTextLn
  , putNewline
  , displayLazyBS
  , fromRawBytes
  , toBase58Text
  , textShow
  , wrapIn
  , module Web.UCAN.Internal.UTF8
  ) where

import           Data.Binary            hiding (encode)

import           RIO
import qualified RIO.ByteString         as Strict
import qualified RIO.ByteString.Lazy    as Lazy

import           Fission.Text.Encoded

import           Web.UCAN.Internal.UTF8 hiding (toBase58Text)
import qualified Web.UCAN.Internal.UTF8 as UTF8

displayLazyBS :: Display a => a -> Lazy.ByteString
displayLazyBS = Lazy.fromStrict . encodeUtf8 . textDisplay

fromRawBytes :: [Word8] -> Text
fromRawBytes = decodeUtf8Lenient . Strict.pack

toBase58Text :: Strict.ByteString -> 'Base58_BTC `Encoded` Text
toBase58Text = Encoded . UTF8.toBase58Text

-- | Show text.
textShow :: Show a => a -> Text
textShow = textDisplay . displayShow

-- | Helper for printing 'Text' to a console
putText :: MonadIO m => Text -> m ()
putText = Strict.putStr . encodeUtf8

-- | Helper for printing Text' to a console with a newline at the end
putTextLn :: MonadIO m => Text -> m ()
putTextLn txt = putText $ txt <> "\n"

putNewline :: MonadIO m => m ()
putNewline = putText "\n"

-- | Wrap text with some other piece of text
wrapIn :: Semigroup s => s -> s -> s
wrapIn wrapper txt = wrapper <> txt <> wrapper
