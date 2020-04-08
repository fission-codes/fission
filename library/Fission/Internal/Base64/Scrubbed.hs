-- | "Scrubbed bytes" don't show up in the console

module Fission.Internal.Base64.Scrubbed
  ( scrub
  , scrubB64

  -- * Reexports
 
  , module Data.ByteString.Base64
  ) where

import qualified RIO.ByteString         as BS
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteArray         as BA

import           Fission.Prelude

-- Reexports

import           Data.ByteString.Base64

-- | Scrub incoming base64-encoded bytes
scrubB64 :: ByteString -> BA.ScrubbedBytes
scrubB64 = scrub . BS64.decodeLenient

scrub :: ByteString -> BA.ScrubbedBytes
scrub = BA.pack . BS.unpack
