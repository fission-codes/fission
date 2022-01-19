module Web.Ucan.Validation
  ( check
  , check'
  , pureChecks
  , checkTime
  , checkSignature
  , checkEd25519Signature
  , checkRSA2048Signature
  ) where

import           Crypto.Hash.Algorithms         (SHA256 (..))
import qualified Crypto.PubKey.Ed25519          as Crypto.Ed25519
import qualified Crypto.PubKey.RSA.PKCS15       as Crypto.RSA.PKCS

import           RIO                            hiding (exp)
import           RIO.Time

import           Control.Monad.Time

import           Crypto.Key.Asymmetric          as Key
import           Web.DID.Types                  as User
import           Web.SemVer.Types

import           Web.Ucan.Resolver              as Proof

import           Web.Ucan.Claims.Error
import           Web.Ucan.Header.Error
import           Web.Ucan.Signature.Error

import           Web.Ucan.Error                 as JWT
import           Web.Ucan.Proof                 as JWT.Proof
import           Web.Ucan.Types                 as JWT

import qualified Web.Ucan.Signature.RS256.Types as RS256
import           Web.Ucan.Signature.Types       as Signature

check ::
  ( Proof.Resolver m fct rsc
  , JWT.Proof.ResourceSemantics rsc
  , MonadTime      m
  )
  => DID
  -> JWT.RawContent
  -> JWT fct rsc
  -> m (Either JWT.Error (JWT fct rsc))
check receiverDID rawContent jwt = do
  now <- currentTime
  case checkTime now jwt of
    Left err ->
      return $ Left err

    Right _  ->
      case checkReceiver receiverDID jwt of
        Left  err -> return $ Left err
        Right _   -> check' rawContent jwt now

check' ::
  ( Proof.Resolver m fct rsc
  , JWT.Proof.ResourceSemantics rsc
  )
  => JWT.RawContent
  -> JWT fct rsc
  -> UTCTime
  -> m (Either JWT.Error (JWT fct rsc))
check' raw jwt now =
  case pureChecks raw jwt of
    Left  err -> return $ Left err
    Right _   -> checkProof now jwt

pureChecks :: JWT.RawContent -> JWT fct rsc -> Either JWT.Error (JWT fct rsc)
pureChecks raw jwt = do
  _ <- checkVersion  jwt
  checkSignature raw jwt

checkReceiver :: DID -> JWT fct rsc -> Either JWT.Error (JWT fct rsc)
checkReceiver recipientDID jwt@JWT {claims = JWT.Claims {receiver}} = do
  if receiver == recipientDID
    then Right jwt
    else Left $ ClaimsError IncorrectReceiver

checkVersion :: JWT fct rsc -> Either JWT.Error (JWT fct rsc)
checkVersion jwt@JWT { header = JWT.Header {uav = SemVer mjr mnr pch}} =
  if mjr == 1 && mnr >= 0 && pch >= 0
    then Right jwt
    else Left $ JWT.HeaderError UnsupportedVersion

checkProof ::
  ( Proof.Resolver m fct rsc
  , JWT.Proof.ResourceSemantics rsc
  )
  => UTCTime -> JWT fct rsc -> m (Either JWT.Error (JWT fct rsc))
checkProof now jwt@JWT {claims = Claims {proof}} =
  case proof of
    RootCredential ->
      return $ Right jwt

    Reference cid ->
      Proof.resolve cid >>= \case
        Left err ->
          return . Left . JWT.ClaimsError . ProofError . JWT.Proof.ResolverError $ err

        Right (rawProof, proofJWT) ->
          check' rawProof proofJWT now <&> \case
            Left err -> Left err
            Right _  -> checkDelegate proofJWT

    Nested rawProof proofJWT ->
      check' rawProof proofJWT now <&> \case
        Left err -> Left err
        Right _  -> checkDelegate proofJWT

    where
      checkDelegate proofJWT =
        case JWT.Proof.delegatedInBounds jwt proofJWT of
          Left err -> Left . JWT.ClaimsError $ ProofError err
          Right _  -> Right jwt

checkTime :: UTCTime -> JWT fct rsc -> Either JWT.Error (JWT fct rsc)
checkTime now jwt@JWT {claims = JWT.Claims { exp, nbf }} =
  if | now > exp -> Left $ JWT.ClaimsError Expired
     | now < nbf -> Left $ JWT.ClaimsError TooEarly
     | otherwise -> Right jwt

checkSignature :: JWT.RawContent -> JWT fct rsc -> Either JWT.Error (JWT fct rsc)
checkSignature rawContent jwt@JWT {sig} =
  case sig of
    Signature.Ed25519 _        -> checkEd25519Signature rawContent jwt
    Signature.RS256   rs256Sig -> checkRSA2048Signature rawContent jwt rs256Sig

checkRSA2048Signature ::
     JWT.RawContent
  -> JWT fct rsc
  -> RS256.Signature
  -> Either JWT.Error (JWT fct rsc)
checkRSA2048Signature (JWT.RawContent raw) jwt@JWT {..} (RS256.Signature innerSig) = do
  case publicKey of
    RSAPublicKey pk ->
      if Crypto.RSA.PKCS.verify (Just SHA256) pk content innerSig
        then Right jwt
        else Left $ JWT.SignatureError SignatureDoesNotMatch

    _ ->
      Left $ JWT.SignatureError InvalidPublicKey

  where
    content = encodeUtf8 raw
    Claims {sender = User.DID {publicKey}} = claims

checkEd25519Signature :: JWT.RawContent -> JWT fct rsc -> Either JWT.Error (JWT fct rsc)
checkEd25519Signature (JWT.RawContent raw) jwt@JWT {..} =
  case (publicKey, sig) of
    (Ed25519PublicKey pk, Signature.Ed25519 edSig) ->
      if Crypto.Ed25519.verify pk (encodeUtf8 raw) edSig
        then Right jwt
        else Left $ JWT.SignatureError SignatureDoesNotMatch

    (_, _) ->
      Left $ JWT.SignatureError InvalidPublicKey

  where
    Claims {sender = User.DID {publicKey}} = claims
