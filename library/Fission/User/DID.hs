module Fission.User.DID
  ( fromPubkey
  , toPubkey
  , DID (..) -- so just this now :thinking:
  )
  where

import qualified RIO.Text as Text

import           Fission.Prelude
import           Fission.PublicKey.Types
 
import           Fission.User.DID.Types as DID

import           Fission.Web.Auth.JWT.Error as JWT
-- import qualified Fission.Internal.Crypto    as Crypto

  -- Hmmmmmm just the constructor now?
fromPubkey :: PublicKey -> DID
fromPubkey pk = DID pk DID.Ed25519 DID.Key -- ("did:key:" <> pk))
-- decodeUtf8Lenient (Crypto.toBase64 pubkey))

toPubkey :: PublicKey -> Either JWT.Error ByteString
toPubkey (PublicKey did) =
  case Text.stripPrefix "did:key:" did of
    Nothing     -> Left JWT.DIDNotSupported
    Just pubkey -> Right (encodeUtf8 pubkey)
