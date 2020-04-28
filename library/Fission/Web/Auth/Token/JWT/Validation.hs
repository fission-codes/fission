module Fission.Web.Auth.Token.JWT.Validation
  ( check
  , check'
  , pureChecks
  , checkTime
  , checkSignature
  , checkEd25519Signature
  , checkRSA2048Signature
  ) where

import           Crypto.Error
import           Crypto.Hash.Algorithms (SHA256 (..))
import qualified Crypto.PubKey.Ed25519    as Crypto.Ed25519
import qualified Crypto.PubKey.RSA.PKCS15 as Crypto.RSA.PKCS

import           Control.Monad.Trans.Except

import           Fission.Prelude
import           Fission.SemVer.Types

import           Fission.Key  as Key
import qualified Fission.User as User

import           Fission.Authorization.ServerDID.Class
 
import           Fission.Web.Auth.Token.JWT.Resolver as Proof

import           Fission.Web.Auth.Token.JWT.Header.Error
import           Fission.Web.Auth.Token.JWT.Claims.Error
import           Fission.Web.Auth.Token.JWT.Signature.Error

import           Fission.Web.Auth.Token.JWT.Proof as JWT.Proof
 
import           Fission.Web.Auth.Token.JWT.Signature.Types       as Signature
import qualified Fission.Web.Auth.Token.JWT.Signature.RS256.Types as RS256
 
import           Fission.Web.Auth.Token.JWT       as JWT
import           Fission.Web.Auth.Token.JWT.Error as JWT

check ::
  ( Proof.Resolver m
  , ServerDID      m
  , MonadTime      m
  )
  => Text
  -> JWT
  -> m (Either JWT.Error JWT)
check rawContent jwt = check' rawContent jwt =<< currentTime

-- NOTE: Despite also having an effect, this is broken out
-- so that we don't need to lookup time repeatedly in recursive checks
check' ::
  ( ServerDID      m
  , Proof.Resolver m
  )
  => Text
  -> JWT
  -> UTCTime
  -> m (Either JWT.Error JWT)
check' raw jwt now =
  case pureChecks raw jwt now of
    Left  err -> return $ Left err
    Right _   -> runExceptT do
      void . ExceptT $ checkReceiver jwt
      ExceptT $ checkProof now       jwt

pureChecks ::
     Text
  -> JWT
  -> UTCTime
  -> Either JWT.Error JWT
pureChecks raw jwt now = do
  checkVersion       jwt
  checkSignature raw jwt
  checkTime      now jwt

checkReceiver :: ServerDID m => JWT -> m (Either JWT.Error JWT)
checkReceiver jwt@JWT {claims = JWT.Claims {receiver}} = do
  serverDID <- getServerDID
  return if receiver == serverDID
    then Right jwt
    else Left $ ClaimsError IncorrectReceiver

checkVersion :: JWT -> Either JWT.Error JWT
checkVersion jwt@JWT { header = JWT.Header {uav = SemVer mjr mnr pch}} =
  if mjr < 1 && mnr <= 1 && pch <= 0
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
  case (now > exp, nbf) of
    (True, _) ->
      Left $ JWT.ClaimsError Expired

    (_, Just nbf') ->
      if now < nbf'
        then Left $ JWT.ClaimsError TooEarly
        else Right jwt
     
    _ ->
      Right jwt

checkSignature :: Text -> JWT -> Either JWT.Error JWT
checkSignature rawContent jwt@JWT {sig} =
  case sig of
    Signature.Ed25519 _        -> checkEd25519Signature rawContent jwt
    Signature.RS256   rs256Sig -> checkRSA2048Signature rawContent jwt rs256Sig

checkRSA2048Signature :: Text -> JWT -> RS256.Signature -> Either JWT.Error JWT
checkRSA2048Signature rawContent jwt@JWT {..} (RS256.Signature innerSig) = do
  case publicKey of
    RSAPublicKey pk ->
      if Crypto.RSA.PKCS.verify (Just SHA256) pk content innerSig
        then Right jwt
        else Left $ JWT.SignatureError SignatureDoesNotMatch

    _ ->
      Left $ JWT.SignatureError InvalidPublicKey
 
  where
    content = encodeUtf8 rawContent
    Claims {sender = User.DID {publicKey}} = claims

checkEd25519Signature :: Text -> JWT -> Either JWT.Error JWT
checkEd25519Signature rawContent jwt@JWT {..} =
  case (publicKey, Crypto.Ed25519.signature sig) of
    (Ed25519PublicKey pk, CryptoPassed sig') ->
      if Crypto.Ed25519.verify pk (encodeUtf8 rawContent) sig'
        then Right jwt
        else Left $ JWT.SignatureError SignatureDoesNotMatch

    (_, CryptoFailed _) ->
      Left $ JWT.SignatureError InvalidSignature
 
    (_, _) ->
      Left $ JWT.SignatureError InvalidPublicKey
    
  where
    Claims {sender = User.DID {publicKey}} = claims
