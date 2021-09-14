-- | Module for DEPRECATED Ed25519 DID encoding format
module Fission.User.DID.Oldstyle.Types (Oldstyle (..)) where

import qualified RIO.ByteString                as BS

import           Fission.Prelude

import qualified Fission.Internal.UTF8         as UTF8

import           Fission.Key                   as Key
import           Fission.User.DID.Method.Types
import           Fission.User.DID.Types

-- | DEPRECATED Encoding of oldstyle Ed25519 DIDs. Manual use only
newtype Oldstyle = Oldstyle { did :: DID }
  deriving stock (Show, Eq)

instance Display Oldstyle where
  textDisplay Oldstyle {did = DIDKey (Ed25519PublicKey ed)} =
    mconcat
      [ "did:key:z"
      , forgetEncoding . UTF8.toBase58Text $ BS.pack (0xed : 0x01 : BS.unpack (encodeUtf8 $ textDisplay ed))
      ]

  textDisplay Oldstyle {did} =
    textDisplay did
