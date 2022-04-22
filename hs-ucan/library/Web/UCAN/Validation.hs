module Web.UCAN.Validation
  ( checkPure
  , checkDelegation
  , checkTime
  , checkReceiver
  , checkFacts
  ) where

import           Crypto.Hash.Algorithms         (SHA256 (..))
import qualified Crypto.PubKey.Ed25519          as Crypto.Ed25519
import qualified Crypto.PubKey.RSA.PKCS15       as Crypto.RSA.PKCS

import           RIO                            hiding (exp)
import           RIO.Time

import           Crypto.Key.Asymmetric          as Key
import           Web.DID.Types                  as DID

import           Web.UCAN.Claims.Error
import           Web.UCAN.Header.Error
import           Web.UCAN.Signature.Error

import           Web.UCAN                       as UCAN
import           Web.UCAN.Error                 as UCAN
import qualified Web.UCAN.Proof.Error           as Proof
import qualified Web.UCAN.Signature.RS256.Types as RS256
import           Web.UCAN.Signature.Types       as Signature



checkPure :: UCAN fct res abl -> Either UCAN.Error ()
checkPure ucan = do
  checkVersion ucan
  checkSignature ucan


checkReceiver :: DID -> UCAN fct res abl -> Either UCAN.Error ()
checkReceiver recipientDID UCAN{ claims = UCAN.Claims {receiver} } = do
  if receiver == recipientDID
    then Right ()
    else Left $ ClaimsError IncorrectReceiver


checkTime :: UTCTime -> UCAN fct res abl -> Either UCAN.Error (UCAN fct res abl)
checkTime now ucan@UCAN {claims = UCAN.Claims { expiration, notBefore }} =
  if now > expiration
    then Left $ UCAN.ClaimsError Expired
    else case notBefore of
      Nothing  -> Right ucan
      Just nbf ->
        if now < nbf
          then Left $ UCAN.ClaimsError TooEarly
          else Right ucan


checkDelegation :: UCAN fct res abl -> UCAN fct res abl -> Either UCAN.Error ()
checkDelegation UCAN.UCAN{ header = headerFrom, claims = claimsFrom } UCAN.UCAN{ header = headerTo, claims = claimsTo } = do
  senderReceiverMatch
  expirationBeforeNotBefore
  notBeforeAfterExpiration
  versionIsIncreasing
  where
    receiver = UCAN.receiver claimsFrom
    sender = UCAN.sender claimsTo

    senderReceiverMatch =
      unless (sender == receiver) do
        Left $ UCAN.ProofError $ Proof.IssuerAudienceMismatch sender receiver

    expirationBeforeNotBefore =
      case (UCAN.expiration claimsFrom, UCAN.notBefore claimsTo) of
        (exp, Just nbf) | exp <= nbf ->
          Left $ UCAN.ProofError $ Proof.NotBeforeProofExpired nbf exp

        _ ->
          Right ()

    notBeforeAfterExpiration =
      case (UCAN.notBefore claimsFrom, UCAN.expiration claimsTo) of
        (Just nbf, exp) | nbf >= exp ->
          Left $ UCAN.ProofError $ Proof.ExpiresAfterNotBefore exp nbf

        _ ->
          Right ()

    versionIsIncreasing =
      unless (UCAN.ucv headerFrom >= UCAN.ucv headerTo) do
        Left $ UCAN.ProofError $ Proof.DecreasingVersionInChain (UCAN.ucv headerTo) (UCAN.ucv headerFrom)


-- 🤓
checkFacts :: UCAN fct res abl -> ([fct] -> Either err ()) -> Either err ()
checkFacts ucan factChecker = ucan & claims & facts & factChecker


-- ㊙️


checkVersion :: UCAN fct res abl -> Either UCAN.Error ()
checkVersion UCAN{ header = UCAN.Header {ucv} } =
  if isSupportedVersion ucv
    then Right ()
    else Left $ UCAN.HeaderError UnsupportedVersion


checkSignature :: UCAN fct res abl -> Either UCAN.Error ()
checkSignature UCAN {..} =
  case (publicKey, signature) of
    (RSAPublicKey pk, Signature.RS256 (RS256.Signature innerSig)) ->
      if Crypto.RSA.PKCS.verify (Just SHA256) pk content innerSig
        then Right ()
        else Left $ UCAN.SignatureError SignatureDoesNotMatch

    (Ed25519PublicKey pk, Signature.Ed25519 edSig) ->
      if Crypto.Ed25519.verify pk content edSig
        then Right ()
        else Left $ UCAN.SignatureError SignatureDoesNotMatch

    (_, _) ->
      Left $ UCAN.SignatureError InvalidPublicKey

  where
    content = encodeUtf8 $ unRawContent signedData
    Claims {sender = DID.Key publicKey} = claims
