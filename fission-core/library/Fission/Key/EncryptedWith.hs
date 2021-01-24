module Fission.Key.EncryptedWith
  ( fromBase64
  , toBase64
  --
  , module Fission.Key.EncryptedWith.Types
  ) where

import qualified Data.ByteString.Base64          as Base64
import qualified RIO.ByteString.Lazy             as Lazy

import           Fission.Prelude

import           Fission.Key.EncryptedWith.Types

fromBase64 :: EncryptedWith cipher msg -> EncryptedWith cipher msg
fromBase64 (EncryptedPayload lbs) = EncryptedPayload . Lazy.fromStrict . Base64.decodeLenient $ Lazy.toStrict lbs

toBase64 :: EncryptedWith cipher msg -> EncryptedWith cipher msg
toBase64 (EncryptedPayload lbs) = EncryptedPayload . Lazy.fromStrict . Base64.encode $ Lazy.toStrict lbs
