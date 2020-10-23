module Fission.Web.Auth.Token.UCAN.Signature
  ( signEdDSA
  , signRS256
  ) where

import           Crypto.Hash.Algorithms                           (SHA256 (..))
import           Crypto.Random                                    (MonadRandom (..))

import qualified Crypto.PubKey.RSA                                as RSA
import qualified Crypto.PubKey.RSA.PKCS15                         as RSA.PKCS15

import qualified Crypto.PubKey.Ed25519                            as Ed25519

import qualified Fission.Internal.Base64.URL                      as B64.URL
import           Fission.Prelude

import           Fission.Key                                      as Key

import           Fission.Web.Auth.Token.JWT.Header.Types          (Header (..))
import           Fission.Web.Auth.Token.JWT.Signature             as Signature
import qualified Fission.Web.Auth.Token.JWT.Signature.RS256.Types as RS256

signEdDSA ::
  ToJSON claims
  => Header
  -> claims
  -> Ed25519.SecretKey
  -> Signature.Signature
signEdDSA header claims sk =
  Signature.Ed25519 . Key.signWith sk . encodeUtf8 $ B64.URL.encodeJWT header claims

signRS256 ::
  ( ToJSON claims
  , MonadRandom m
  )
  => Header
  -> claims
  -> RSA.PrivateKey
  -> m (Either RSA.Error Signature.Signature)
signRS256 header claims sk =
  RSA.PKCS15.signSafer (Just SHA256) sk (encodeUtf8 $ B64.URL.encodeJWT header claims) <&> \case
    Left err  -> Left err
    Right sig -> Right . Signature.RS256 $ RS256.Signature sig
