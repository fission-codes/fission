module Web.UCAN.Validation
  ( check
  , check'
  , pureChecks
  , checkTime
  , checkSignature
  , checkEd25519Signature
  , checkRSA2048Signature
  ) where

import           Crypto.Hash
import qualified Crypto.PubKey.Ed25519                  as Crypto.Ed25519
import qualified Crypto.PubKey.RSA.PKCS15               as Crypto.RSA.PKCS
import qualified Crypto.Secp256k1

import           RIO                                    hiding (exp)
import           RIO.ByteString                         as BS
import           RIO.Time

import qualified Data.ByteArray                         as BA
import           Data.Aeson

import           Control.Monad.Time

import           Crypto.Key.Asymmetric                  as Key
import           Web.DID.Types                          as DID
import           Web.SemVer.Types

import           Web.UCAN.Resolver                      as Proof

import           Web.UCAN.Claims.Error
import           Web.UCAN.Header.Error
import           Web.UCAN.Signature.Error

import           Web.UCAN                               as UCAN
import           Web.UCAN.Error                         as UCAN
import           Web.UCAN.Proof                         as UCAN.Proof

import qualified Web.UCAN.Signature.RS256.Types         as RS256
import qualified Web.UCAN.Signature.Secp256k1.Types     as Secp256k1
import           Web.UCAN.Signature.Types               as Signature
import qualified Web.UCAN.Signature.Secp256k1.Ethereum  as Ethereum

check ::
  ( Proof.Resolver m
  , MonadTime      m
  , FromJSON fct
  , FromJSON rsc
  , FromJSON ptc
  , UCAN.Proof.DelegationSemantics rsc
  , UCAN.Proof.DelegationSemantics ptc
  )
  => DID
  -> UCAN.RawContent
  -> UCAN fct rsc ptc
  -> m (Either UCAN.Error (UCAN fct rsc ptc))
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
  , FromJSON ptc
  , UCAN.Proof.DelegationSemantics rsc
  , UCAN.Proof.DelegationSemantics ptc
  )
  => UCAN.RawContent
  -> UCAN fct rsc ptc
  -> UTCTime
  -> m (Either UCAN.Error (UCAN fct rsc ptc))
check' raw ucan now =
  case pureChecks raw ucan of
    Left  err -> return $ Left err
    Right _   -> checkProof now ucan

pureChecks :: UCAN.RawContent -> UCAN fct rsc ptc -> Either UCAN.Error (UCAN fct rsc ptc)
pureChecks raw ucan = do
  _ <- checkVersion  ucan
  checkSignature raw ucan

checkReceiver :: DID -> UCAN fct rsc ptc -> Either UCAN.Error (UCAN fct rsc ptc)
checkReceiver recipientDID ucan@UCAN {claims = UCAN.Claims {receiver}} = do
  if receiver == recipientDID
    then Right ucan
    else Left $ ClaimsError IncorrectReceiver

checkVersion :: UCAN fct rsc ptc -> Either UCAN.Error (UCAN fct rsc ptc)
checkVersion ucan@UCAN { header = UCAN.Header {uav = SemVer mjr mnr pch}} =
  if mjr == 1 && mnr >= 0 && pch >= 0
    then Right ucan
    else Left $ UCAN.HeaderError UnsupportedVersion

checkProof ::
  ( Proof.Resolver m
  , FromJSON fct
  , FromJSON rsc
  , FromJSON ptc
  , UCAN.Proof.DelegationSemantics rsc
  , UCAN.Proof.DelegationSemantics ptc
  )
  => UTCTime -> UCAN fct rsc ptc -> m (Either UCAN.Error (UCAN fct rsc ptc))
checkProof now ucan@UCAN {claims = Claims {proof}} =
  case proof of
    RootCredential ->
      return $ Right ucan

    Reference cid ->
      Proof.resolve cid >>= \case
        Left err ->
          return . Left . UCAN.ClaimsError . ProofError $ UCAN.Proof.ResolverError err

        Right proofBS -> do
          case UCAN.parse proofBS of
            Right proofUCAN ->
              check' (UCAN.contentOf (decodeUtf8Lenient proofBS)) proofUCAN now <&> \case
                Left err -> Left err
                Right _  -> checkDelegate proofUCAN

            Left (InvalidJWT reason _) ->
              return . Left $ UCAN.ParseError reason

            Left _ ->
              return . Left . UCAN.ParseError $ "unknown: " <> decodeUtf8Lenient proofBS

    Nested rawProof proofUCAN ->
      check' rawProof proofUCAN now <&> \case
        Left err -> Left err
        Right _  -> checkDelegate proofUCAN

    where
      checkDelegate proofUCAN =
        case UCAN.Proof.delegatedInBounds ucan proofUCAN of
          Left err -> Left . UCAN.ClaimsError $ ProofError err
          Right _  -> Right ucan

checkTime :: UTCTime -> UCAN fct rsc ptc -> Either UCAN.Error (UCAN fct rsc ptc)
checkTime now ucan@UCAN {claims = UCAN.Claims { exp, nbf }} =
  if | now > exp -> Left $ UCAN.ClaimsError Expired
     | now < nbf -> Left $ UCAN.ClaimsError TooEarly
     | otherwise -> Right ucan

checkSignature :: UCAN.RawContent -> UCAN fct rsc ptc -> Either UCAN.Error (UCAN fct rsc ptc)
checkSignature rawContent ucan@UCAN {sig} =
  case sig of
    Signature.Ed25519   _        -> checkEd25519Signature rawContent ucan
    Signature.RS256     rs256Sig -> checkRSA2048Signature rawContent ucan rs256Sig
    Signature.Secp256k1 secpSig  -> checkSecp256k1Signature rawContent ucan secpSig

checkRSA2048Signature ::
     UCAN.RawContent
  -> UCAN fct rsc ptc
  -> RS256.Signature
  -> Either UCAN.Error (UCAN fct rsc ptc)
checkRSA2048Signature (UCAN.RawContent raw) ucan@UCAN {..} (RS256.Signature innerSig) = do
  case publicKey of
    RSAPublicKey pk ->
      if Crypto.RSA.PKCS.verify (Just SHA256) pk content innerSig
        then Right ucan
        else Left $ UCAN.SignatureError SignatureDoesNotMatch

    _ ->
      Left $ UCAN.SignatureError InvalidPublicKey

  where
    content = encodeUtf8 raw
    Claims {sender = DID.Key publicKey} = claims

checkEd25519Signature :: UCAN.RawContent -> UCAN fct rsc ptc -> Either UCAN.Error (UCAN fct rsc ptc)
checkEd25519Signature (UCAN.RawContent raw) ucan@UCAN {..} =
  case (publicKey, sig) of
    (Ed25519PublicKey pk, Signature.Ed25519 edSig) ->
      if Crypto.Ed25519.verify pk (encodeUtf8 raw) edSig
        then Right ucan
        else Left $ UCAN.SignatureError SignatureDoesNotMatch

    (_, _) ->
      Left $ UCAN.SignatureError InvalidPublicKey

  where
    Claims {sender = DID.Key publicKey} = claims

checkSecp256k1Signature ::
     UCAN.RawContent
  -> UCAN fct rsc ptc
  -> Secp256k1.Signature
  -> Either UCAN.Error (UCAN fct rsc ptc)
checkSecp256k1Signature (UCAN.RawContent raw) ucan@UCAN {..} (Secp256k1.Signature innerSig) = do
  case publicKey of
    Secp256k1PublicKey pk ->
      -- `innerSig` is actually a recoverable signature,
      -- but since Ethereum (or Metamask?) often has a `v` value
      -- of `27 + actual v value` for historical reasons, we ignore that value here.
      --
      -- Recoverable signature: 65 bytes
      -- Regular signature: 64 bytes
      case Crypto.Secp256k1.importSignature (BS.take 64 innerSig) of
        Just secpSig ->
          let
            hashedContent =
              BS.pack . BA.unpack . Ethereum.hashWithPrefix $ content
          in
          if Crypto.Secp256k1.ecdsaVerify hashedContent pk secpSig
            then Right ucan
            else Left $ UCAN.SignatureError SignatureDoesNotMatch

        Nothing ->
          Left $ UCAN.SignatureError InvalidPublicKey

    _ ->
      Left $ UCAN.SignatureError InvalidPublicKey

  where
    content = encodeUtf8 raw
    Claims {sender = DID.Key publicKey} = claims