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
import qualified Crypto.PubKey.Ed25519                            as Ed25519
import qualified Crypto.PubKey.RSA.PKCS15                         as Crypto.RSA.PKCS

import           Fission.Prelude

import           Fission.Key                                      as Key
import           Fission.SemVer.Types
import           Fission.User.DID                                 as DID

import           Fission.Web.Auth.Token.JWT.Resolver              as Proof

import           Fission.Web.Auth.Token.JWT.Claims.Error
import           Fission.Web.Auth.Token.JWT.Header.Error
import           Fission.Web.Auth.Token.JWT.Signature.Error

import           Fission.Web.Auth.Token.JWT.Error                 as JWT
import           Fission.Web.Auth.Token.JWT.Proof                 as JWT.Proof
import           Fission.Web.Auth.Token.JWT.Types                 as JWT

import qualified Fission.Web.Auth.Token.JWT.Signature.RS256.Types as RS256
import           Fission.Web.Auth.Token.JWT.Signature.Types       as Signature

check ::
  ( Proof.Resolver m
  , MonadTime      m
  )
  => DID
  -> JWT.RawContent
  -> JWT
  -> m (Either JWT.Error JWT)
check receiverDID rawContent jwt = do
  now <- currentTime

  ionPKs <-
    case receiverDID of
      DID.Key _      -> return []
      DID.ION ionTxt -> return [ undefined ] -- FIXME

  case checkTime now jwt of
    Left err ->
      return $ Left err

    Right _  ->
      case checkReceiver receiverDID jwt of
        Left  err -> return $ Left err
        Right _   -> check' rawContent jwt ionPKs now

check' ::
  Proof.Resolver m
  => JWT.RawContent
  -> JWT
  -> [Ed25519.PublicKey]
  -> UTCTime
  -> m (Either JWT.Error JWT)
check' raw jwt ionPKs now =
  case pureChecks raw jwt ionPKs of
    Left  err -> return $ Left err
    Right _   -> checkProof now jwt ionPKs

pureChecks :: JWT.RawContent -> JWT -> [Ed25519.PublicKey] -> Either JWT.Error JWT
pureChecks raw jwt ionPKs = do
  _ <- checkVersion  jwt
  checkSignature raw jwt ionPKs

checkReceiver :: DID -> JWT -> Either JWT.Error JWT
checkReceiver recipientDID jwt@JWT {claims = JWT.Claims {receiver}} = do
  if receiver == recipientDID
    then Right jwt
    else Left $ ClaimsError IncorrectReceiver

checkVersion :: JWT -> Either JWT.Error JWT
checkVersion jwt@JWT { header = JWT.Header {uav = SemVer mjr mnr pch}} =
  if mjr == 1 && mnr >= 0 && pch >= 0
    then Right jwt
    else Left $ JWT.HeaderError UnsupportedVersion

checkProof :: Proof.Resolver m => UTCTime -> JWT -> [Ed25519.PublicKey] -> m (Either JWT.Error JWT)
checkProof now jwt@JWT {claims = Claims {proof}} ionPKs =
  case proof of
    RootCredential ->
      return $ Right jwt

    Reference cid ->
      Proof.resolve cid >>= \case
        Left err ->
          return . Left . JWT.ClaimsError . ProofError . JWT.Proof.ResolverError $ err

        Right (rawProof, proofJWT) ->
          check' rawProof proofJWT ionPKs now <&> \case
            Left err -> Left err
            Right _  -> checkDelegate proofJWT

    Nested rawProof proofJWT ->
      check' rawProof proofJWT ionPKs now <&> \case
        Left err -> Left err
        Right _  -> checkDelegate proofJWT

    where
      checkDelegate proofJWT =
        case JWT.Proof.delegatedInBounds jwt proofJWT of
          Left err -> Left . JWT.ClaimsError $ ProofError err
          Right _  -> Right jwt

checkTime :: UTCTime -> JWT -> Either JWT.Error JWT
checkTime now jwt@JWT {claims = JWT.Claims { exp, nbf }} =
  if | now > exp -> Left $ JWT.ClaimsError Expired
     | now < nbf -> Left $ JWT.ClaimsError TooEarly
     | otherwise -> Right jwt

checkSignature :: JWT.RawContent -> JWT -> [Ed25519.PublicKey] -> Either JWT.Error JWT
checkSignature rawContent jwt@JWT {sig} ionPKs =
  case sig of
    Signature.Ed25519 _        -> checkEd25519Signature rawContent jwt ionPKs
    Signature.RS256   rs256Sig -> checkRSA2048Signature rawContent jwt rs256Sig

checkRSA2048Signature ::
     JWT.RawContent
  -> JWT
  -> RS256.Signature
  -> Either JWT.Error JWT
checkRSA2048Signature (JWT.RawContent raw) jwt@JWT {..} (RS256.Signature innerSig) = do
  case publicKey of
    Right (RSAPublicKey pk) ->
      if Crypto.RSA.PKCS.verify (Just SHA256) pk content innerSig
        then Right jwt
        else Left $ JWT.SignatureError SignatureDoesNotMatch

    _ ->
      Left $ JWT.SignatureError InvalidPublicKey

  where
    content = encodeUtf8 raw

    publicKey =
      case claims |> sender of
        DID.ION _  -> Left undefined -- FIXME
        DID.Key pk -> Right pk

checkEd25519Signature :: JWT.RawContent -> JWT -> [Ed25519.PublicKey] -> Either JWT.Error JWT
checkEd25519Signature (JWT.RawContent raw) jwt@JWT {..} ionPKs =
  case (sender, sig) of
    (DID.Key (Ed25519PublicKey pk), Signature.Ed25519 edSig) ->
      checkEd edSig pk

    (DID.ION _, Signature.Ed25519 edSig) ->
      case filter isRight (checkEd edSig <$> ionPKs) of
        (Right jwt' : _) -> Right jwt'
        _                -> Left $ JWT.SignatureError InvalidPublicKey

    _ ->
      Left $ JWT.SignatureError InvalidPublicKey

  where
    Claims {sender} = claims

    checkEd :: Ed25519.Signature -> Ed25519.PublicKey -> Either JWT.Error JWT
    checkEd edSig pk =
      if Ed25519.verify pk (encodeUtf8 raw) edSig
        then Right jwt
        else Left $ JWT.SignatureError SignatureDoesNotMatch
