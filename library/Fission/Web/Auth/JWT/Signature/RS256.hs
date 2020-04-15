module Fission.Web.Auth.JWT.Signature.RS256
  ( sign
  , module Fission.Web.Auth.JWT.Signature.RS256.Types
  ) where

import           Crypto.Random          (MonadRandom (..))
import           Crypto.Hash.Algorithms (SHA256 (..))
 
import qualified Crypto.PubKey.RSA        as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA.PKCS15
 
import           Fission.Prelude
import qualified Fission.Internal.Base64.URL as B64.URL

import           Fission.Web.Auth.JWT.Signature.RS256.Types
import qualified Fission.Web.Auth.JWT.Signature.RS256.Types as RS256

import qualified Fission.Web.Auth.JWT.Header.Types    as JWT
import qualified Fission.Web.Auth.JWT.Claims.Types    as JWT
import qualified Fission.Web.Auth.JWT.Signature.Types as JWT.Sig

sign ::
  MonadRandom m
  => JWT.Header
  -> JWT.Claims
  -> RSA.PrivateKey
  -> m (Either RSA.Error JWT.Sig.Signature)
sign header claims sk =
  RSA.PKCS15.signSafer (Just SHA256) sk (B64.URL.encodeJWT header claims) <&> \case
    Left err  -> Left err
    Right sig -> Right . JWT.Sig.RS256 $ RS256.Signature sig
