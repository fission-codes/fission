module Fission.Web.Auth.Token.JWT.Validation
  ( check
  , check'
  , pureChecks
  , checkTime
  , checkSignature
  , checkEd25519Signature
  , checkRSA2048Signature
  ) where

import           Crypto.Hash.Algorithms                           (SHA256 (..))
import qualified Crypto.PubKey.Ed25519                            as Crypto.Ed25519
import qualified Crypto.PubKey.RSA.PKCS15                         as Crypto.RSA.PKCS

import           Fission.Prelude
import           Fission.SemVer.Types

import           Fission.Key                                      as Key
import qualified Fission.User                                     as User

import           Fission.Authorization.ServerDID.Class

import           Fission.Web.Auth.Token.JWT.Resolver              as Proof

import           Fission.Web.Auth.Token.JWT.Claims.Error
import           Fission.Web.Auth.Token.JWT.Header.Error
import           Fission.Web.Auth.Token.JWT.Signature.Error

import           Fission.Web.Auth.Token.JWT.Proof                 as JWT.Proof

import qualified Fission.Web.Auth.Token.JWT.Signature.RS256.Types as RS256
import           Fission.Web.Auth.Token.JWT.Signature.Types       as Signature

import           Fission.Web.Auth.Token.JWT                       as JWT
import           Fission.Web.Auth.Token.JWT.Error                 as JWT

check ::
  ( Proof.Resolver m
  , ServerDID      m
  , MonadTime      m
  )
  => JWT.RawContent
  -> JWT
  -> m (Either JWT.Error JWT)
check rawContent jwt = do
  now <- currentTime
  case checkTime now jwt of
    Left err ->
      return $ Left err

    Right _  ->
      checkReceiver jwt >>= \case
        Left  err -> return $ Left err
        Right _   -> check' rawContent jwt now

check' ::
  ( ServerDID      m
  , Proof.Resolver m
  )
  => JWT.RawContent
  -> JWT
  -> UTCTime
  -> m (Either JWT.Error JWT)
check' raw jwt now =
  case pureChecks raw jwt of
    Left  err -> return $ Left err
    Right _   -> checkProof now jwt

pureChecks ::
     JWT.RawContent
  -> JWT
  -> Either JWT.Error JWT
pureChecks raw jwt = do
  _ <- checkVersion  jwt
  checkSignature raw jwt

checkReceiver :: ServerDID m => JWT -> m (Either JWT.Error JWT)
checkReceiver jwt@JWT {claims = JWT.Claims {receiver}} = do
  serverDID <- getServerDID
  return if receiver == serverDID
    then Right jwt
    else Left $ ClaimsError IncorrectReceiver

checkVersion :: JWT -> Either JWT.Error JWT
checkVersion jwt@JWT { header = JWT.Header {uav = SemVer mjr mnr pch}} =
  if mjr == 1 && mnr >= 0 && pch >= 0
    then Right jwt
    else Left $ JWT.HeaderError UnsupportedVersion

checkProof ::
  ( ServerDID      m
  , Proof.Resolver m
  )
  => UTCTime
  -> JWT
  -> m (Either JWT.Error JWT)
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

checkTime :: UTCTime -> JWT -> Either JWT.Error JWT
checkTime now jwt@JWT {claims = JWT.Claims { exp, nbf }} = do
  if | now > exp -> Left $ JWT.ClaimsError Expired
     | now < nbf -> Left $ JWT.ClaimsError TooEarly
     | otherwise -> Right jwt

checkSignature :: JWT.RawContent -> JWT -> Either JWT.Error JWT
checkSignature rawContent jwt@JWT {sig} =
  case sig of
    Signature.Ed25519 _        -> checkEd25519Signature rawContent jwt
    Signature.RS256   rs256Sig -> checkRSA2048Signature rawContent jwt rs256Sig

checkRSA2048Signature ::
     JWT.RawContent
  -> JWT
  -> RS256.Signature
  -> Either JWT.Error JWT
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

checkEd25519Signature :: JWT.RawContent -> JWT -> Either JWT.Error JWT
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
