module Fission.Web.Auth.JWT.Signature.Ed25519 (sign) where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified RIO.ByteString.Lazy   as Lazy

import           Fission.Prelude
import qualified Fission.Key.Store as Key

import qualified Fission.Web.Auth.JWT.Header.Types    as JWT
import qualified Fission.Web.Auth.JWT.Claims.Types    as JWT
import qualified Fission.Web.Auth.JWT.Signature.Types as JWT.Sig
 
import qualified Fission.Internal.Crypto as Crypto
import qualified Fission.Internal.UTF8 as UTF8

sign ::
     JWT.Header
  -> JWT.Claims
  -> Ed25519.SecretKey
  -> JWT.Sig.Signature
sign header claims sk =
  JWT.Sig.Ed25519 .  Key.signWith sk . -- Crypto.toBase64 .
    -- UTF8.stripOptionalSuffixBS "=" $
    -- UTF8.stripOptionalSuffixBS "=" $
      Lazy.toStrict $
      (encode header <> "." <> encode claims)

  -- where
  --     encodeSig raw =
  --       raw
  --         |> Crypto.toBase64
  --         -- |> stripQuotes
  --         |> decodeUtf8Lenient
  --         |> toURLEncoding
  --         |> encodeUtf8
  --         |> stripPadding

  --     encodeB64 :: ToJSON a => a -> ByteString
  --     encodeB64 jsonable =
  --       jsonable
  --         |> JSON.encode
  --         |> Lazy.toStrict
  --         |> stripQuotes
  --         |> B64URL.encode
  --         |> stripPadding

  --     toURLEncoding :: Text -> Text
  --     toURLEncoding = PText.replace "+" "-" . PText.replace "/" "_"
