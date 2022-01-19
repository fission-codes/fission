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

import           Data.Aeson

import           Control.Monad.Time

import           Crypto.Key.Asymmetric          as Key
import           Web.DID.Types                  as User
import           Web.SemVer.Types

import           Web.Ucan.Resolver              as Proof

import           Web.Ucan.Claims.Error
import           Web.Ucan.Header.Error
import           Web.Ucan.Signature.Error

import           Web.Ucan                       as Ucan
import           Web.Ucan.Error                 as Ucan
import           Web.Ucan.Proof                 as Ucan.Proof

import qualified Web.Ucan.Signature.RS256.Types as RS256
import           Web.Ucan.Signature.Types       as Signature

check ::
  ( Proof.Resolver m
  , FromJSON fct
  , FromJSON rsc
  , Ucan.Proof.ResourceSemantics rsc
  , MonadTime      m
  )
  => DID
  -> Ucan.RawContent
  -> Ucan fct rsc
  -> m (Either Ucan.Error (Ucan fct rsc))
check receiverDID rawContent ucan = do
  now <- currentTime
  case checkTime now ucan of
    Left err ->
      return $ Left err

    Right _  ->
      case checkReceiver receiverDID ucan of
        Left  err -> return $ Left err
        Right _   -> check' rawContent ucan now

check' ::
  ( Proof.Resolver m
  , FromJSON fct
  , FromJSON rsc
  , Ucan.Proof.ResourceSemantics rsc
  )
  => Ucan.RawContent
  -> Ucan fct rsc
  -> UTCTime
  -> m (Either Ucan.Error (Ucan fct rsc))
check' raw ucan now =
  case pureChecks raw ucan of
    Left  err -> return $ Left err
    Right _   -> checkProof now ucan

pureChecks :: Ucan.RawContent -> Ucan fct rsc -> Either Ucan.Error (Ucan fct rsc)
pureChecks raw ucan = do
  _ <- checkVersion  ucan
  checkSignature raw ucan

checkReceiver :: DID -> Ucan fct rsc -> Either Ucan.Error (Ucan fct rsc)
checkReceiver recipientDID ucan@Ucan {claims = Ucan.Claims {receiver}} = do
  if receiver == recipientDID
    then Right ucan
    else Left $ ClaimsError IncorrectReceiver

checkVersion :: Ucan fct rsc -> Either Ucan.Error (Ucan fct rsc)
checkVersion ucan@Ucan { header = Ucan.Header {uav = SemVer mjr mnr pch}} =
  if mjr == 1 && mnr >= 0 && pch >= 0
    then Right ucan
    else Left $ Ucan.HeaderError UnsupportedVersion

checkProof ::
  ( Proof.Resolver m
  , FromJSON fct
  , FromJSON rsc
  , Ucan.Proof.ResourceSemantics rsc
  )
  => UTCTime -> Ucan fct rsc -> m (Either Ucan.Error (Ucan fct rsc))
checkProof now ucan@Ucan {claims = Claims {proof}} =
  case proof of
    RootCredential ->
      return $ Right ucan

    Reference cid ->
      Proof.resolve cid >>= \case
        Left err ->
          return . Left . Ucan.ClaimsError . ProofError . Ucan.Proof.ResolverError $ err

        Right rawProof -> do
          case Ucan.fromRawContent rawProof of
            Left _          -> return $ Left Ucan.ParseError
            Right proofUcan ->
              check' rawProof proofUcan now <&> \case
                Left err -> Left err
                Right _  -> checkDelegate proofUcan

    Nested rawProof proofUcan ->
      check' rawProof proofUcan now <&> \case
        Left err -> Left err
        Right _  -> checkDelegate proofUcan

    where
      checkDelegate proofUcan =
        case Ucan.Proof.delegatedInBounds ucan proofUcan of
          Left err -> Left . Ucan.ClaimsError $ ProofError err
          Right _  -> Right ucan

checkTime :: UTCTime -> Ucan fct rsc -> Either Ucan.Error (Ucan fct rsc)
checkTime now ucan@Ucan {claims = Ucan.Claims { exp, nbf }} =
  if | now > exp -> Left $ Ucan.ClaimsError Expired
     | now < nbf -> Left $ Ucan.ClaimsError TooEarly
     | otherwise -> Right ucan

checkSignature :: Ucan.RawContent -> Ucan fct rsc -> Either Ucan.Error (Ucan fct rsc)
checkSignature rawContent ucan@Ucan {sig} =
  case sig of
    Signature.Ed25519 _        -> checkEd25519Signature rawContent ucan
    Signature.RS256   rs256Sig -> checkRSA2048Signature rawContent ucan rs256Sig

checkRSA2048Signature ::
     Ucan.RawContent
  -> Ucan fct rsc
  -> RS256.Signature
  -> Either Ucan.Error (Ucan fct rsc)
checkRSA2048Signature (Ucan.RawContent raw) ucan@Ucan {..} (RS256.Signature innerSig) = do
  case publicKey of
    RSAPublicKey pk ->
      if Crypto.RSA.PKCS.verify (Just SHA256) pk content innerSig
        then Right ucan
        else Left $ Ucan.SignatureError SignatureDoesNotMatch

    _ ->
      Left $ Ucan.SignatureError InvalidPublicKey

  where
    content = encodeUtf8 raw
    Claims {sender = User.DID {publicKey}} = claims

checkEd25519Signature :: Ucan.RawContent -> Ucan fct rsc -> Either Ucan.Error (Ucan fct rsc)
checkEd25519Signature (Ucan.RawContent raw) ucan@Ucan {..} =
  case (publicKey, sig) of
    (Ed25519PublicKey pk, Signature.Ed25519 edSig) ->
      if Crypto.Ed25519.verify pk (encodeUtf8 raw) edSig
        then Right ucan
        else Left $ Ucan.SignatureError SignatureDoesNotMatch

    (_, _) ->
      Left $ Ucan.SignatureError InvalidPublicKey

  where
    Claims {sender = User.DID {publicKey}} = claims
