module Fission.User.DID
  ( fromPubkey
  , toPubkey
  , module Fission.User.DID.Types
  )
  where

import           Fission.Prelude

import qualified Data.ByteArray (ByteArrayAccess)
import qualified RIO.Text as Text

import           Fission.PublicKey.Types
import           Fission.Web.Auth.JWT.Error as JWT
import qualified Fission.Internal.Crypto    as Crypto

fromPubkey :: ByteArrayAccess a => a -> DID
fromPubkey pubkey = DID ("did:key:" <> decodeUtf8Lenient (Crypto.toBase64 pubkey))

toPubkey :: DID -> Either JWT.Error ByteString
toPubkey (DID did) = 
  case Text.stripPrefix "did:key:" did of
    Nothing     -> Left JWT.DIDNotSupported
    Just pubkey -> Right (encodeUtf8 pubkey)
