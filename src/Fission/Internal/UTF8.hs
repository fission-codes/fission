module Fission.Internal.UTF8 where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

class Textable a where
  encode :: a -> Either UnicodeException Text

instance Textable ByteString where
  encode = decodeUtf8'

instance Textable Lazy.ByteString where
  encode = encode . Lazy.toStrict
