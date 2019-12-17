module Fission.User.DID
  ( fromPubkey
  , toPubkey
  , module Fission.User.DID.Types
  )
  where

import Fission.Prelude

import qualified Data.Text as T
import qualified Data.ByteArray         as BA

import           Fission.User.DID.Types
import           Fission.Web.Auth.JWT.Error as JWT
import qualified Fission.Internal.Crypto as Crypto

fromPubkey :: BA.ByteArrayAccess a => a -> DID
fromPubkey pubkey =
  pubkey
    |> Crypto.toBase64
    |> decodeUtf8Lenient
    |> ("did:key:" <>)
    |> DID

toPubkey :: DID -> Either JWT.Error ByteString
toPubkey (DID did) = 
  case T.stripPrefix "did:key:" did of
    Nothing -> Left JWT.DIDNotSupported
    Just pubkey -> Right <| encodeUtf8 pubkey
