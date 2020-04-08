module Fission.Web.Auth.JWT.Signature.RS256
  ( sign
  , module Fission.Web.Auth.JWT.Signature.RS256.Types
  ) where

import           Crypto.Random          (MonadRandom (..))
import           Crypto.Hash.Algorithms (SHA256 (..))
 
import qualified Crypto.PubKey.RSA        as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA.PKCS15
 
import qualified RIO.ByteString.Lazy as Lazy

import           Fission.Prelude

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
  (encode header <> "." <> encode claims)
    |> Lazy.toStrict
    |> RSA.PKCS15.signSafer (Just SHA256) sk
    |> fmap \case
        Left err  -> Left err
        Right sig -> Right . JWT.Sig.RS256 $ RS256.Signature sig
