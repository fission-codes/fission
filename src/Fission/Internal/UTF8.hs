{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Internal.UTF8
  ( Textable (..)
  , showLazyBS
  , textToLazyBS
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

showLazyBS :: Show a => a -> Lazy.ByteString
showLazyBS = textToLazyBS . textDisplay . displayShow

textToLazyBS :: Text -> Lazy.ByteString
textToLazyBS = Lazy.fromStrict . Text.encodeUtf8
