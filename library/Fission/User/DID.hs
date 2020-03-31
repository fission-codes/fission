module Fission.User.DID
  ( fromPubkey
  , toPubkey
  , DID.Key (..) -- so just this now :thinking:
  )
  where

import qualified RIO.Text as Text

import           Fission.Prelude
import           Fission.PublicKey.Types
 
import qualified Fission.User.DID.Types as DID

import           Fission.Web.Auth.JWT.Error as JWT
-- import qualified Fission.Internal.Crypto    as Crypto

  -- Hmmmmmm just the constructor now?
fromPubkey :: PublicKey -> DID.Key
fromPubkey pk = DID.Key pk -- ("did:key:" <> pk))
-- decodeUtf8Lenient (Crypto.toBase64 pubkey))

toPubkey :: DID.Key -> Either JWT.Error ByteString
toPubkey (DID.Key (PublicKey did)) =
  case Text.stripPrefix "did:key:" did of
    Nothing     -> Left JWT.DIDNotSupported
    Just pubkey -> Right (encodeUtf8 pubkey)
