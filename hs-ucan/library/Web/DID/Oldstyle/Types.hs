-- | Module for DEPRECATED Ed25519 DID encoding format
module Web.DID.Oldstyle.Types (Oldstyle (..)) where

import           RIO
import qualified RIO.ByteString         as BS

import qualified Web.UCAN.Internal.UTF8 as UTF8

import           Crypto.Key.Asymmetric  as Key
import           Web.DID.Method.Types
import           Web.DID.Types


-- | DEPRECATED Encoding of oldstyle Ed25519 DIDs. Manual use only
newtype Oldstyle = Oldstyle { did :: DID }
  deriving stock (Show, Eq)

instance Display Oldstyle where
  textDisplay Oldstyle {did = DID Key (Ed25519PublicKey ed)} =
    mconcat
      [ "did:key:z"
      , UTF8.toBase58Text $ BS.pack (0xed : 0x01 : BS.unpack (encodeUtf8 $ textDisplay ed))
      ]

  textDisplay Oldstyle {did} =
    textDisplay did
