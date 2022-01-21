module Web.DID.Verification
  ( verifySignature
  ) where

import           Crypto.Hash.Algorithms
import qualified Crypto.Key.Asymmetric          as Key
import qualified Crypto.PubKey.Ed25519
import qualified Crypto.PubKey.RSA.PKCS15
import           RIO

import           Web.DID.Types
import qualified Web.UCAN.Signature.Error       as Signature
import qualified Web.UCAN.Signature.RS256.Types as RSA
import           Web.UCAN.Signature.Types       as Signature


verifySignature :: DID -> ByteString -> Signature -> Either Signature.Error Bool
verifySignature DID{ publicKey } signedData signature =
  case (publicKey, signature) of
    (Key.Ed25519PublicKey pk, Signature.Ed25519 sig) ->
      Right $ Crypto.PubKey.Ed25519.verify pk signedData sig

    (Key.RSAPublicKey pk, Signature.RS256 (RSA.Signature sig)) -> do
      Right $ Crypto.PubKey.RSA.PKCS15.verify (Just SHA256) pk signedData sig

    (_, _) ->
      Left Signature.SignatureDoesNotMatch
