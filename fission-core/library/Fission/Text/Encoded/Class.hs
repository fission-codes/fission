module Fission.Text.Encoded.Class (ToEncoded (..)) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import           Fission.Text.Encoded.Types
import           Fission.Text.Encoding.Types

-- | A helper for beinging some native text into the Encoded type and tagging
--   it with that text type's correct default encoding
class ToEncoded txt where
  type NativeEncoding txt :: Encoding
  trackEncoding :: txt -> NativeEncoding txt `Encoded` txt

instance ToEncoded Text where
  type NativeEncoding Text = 'UTF8
  trackEncoding txt = Encoded txt

instance ToEncoded String where
  type NativeEncoding String = 'Octal
  trackEncoding str = Encoded str

instance ToEncoded ByteString where
  type NativeEncoding ByteString = 'Octal
  trackEncoding bs = Encoded bs

instance ToEncoded Lazy.ByteString where
  type NativeEncoding Lazy.ByteString = 'Octal
  trackEncoding lbs = Encoded lbs
