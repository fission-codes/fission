module Web.UCAN.Validation
  ( pureChecks
  -- , check
  -- , check'
  , checkTime
  , checkSignature
  ) where

import           Crypto.Hash.Algorithms         (SHA256 (..))
import qualified Crypto.PubKey.Ed25519          as Crypto.Ed25519
import qualified Crypto.PubKey.RSA.PKCS15       as Crypto.RSA.PKCS

import           RIO                            hiding (exp)
import           RIO.Time

import           Data.Aeson

import           Control.Monad.Time

import           Crypto.Key.Asymmetric          as Key
import           Web.DID.Types                  as DID
import           Web.SemVer.Types

import           Web.UCAN.Resolver              as Proof

import           Web.UCAN.Claims.Error
import           Web.UCAN.Header.Error
import           Web.UCAN.Signature.Error

import           Web.UCAN                       as UCAN
import           Web.UCAN.Error                 as UCAN
import           Web.UCAN.Proof                 as UCAN.Proof

import           Web.UCAN.Header
import qualified Web.UCAN.Signature.RS256.Types as RS256
import           Web.UCAN.Signature.Types       as Signature


-- check ::
--   ( Proof.Resolver m
--   , MonadTime      m
--   , FromJSON fct
--   , FromJSON cap
--   , UCAN.Proof.DelegationSemantics cap
--   )
--   => DID
--   -> UCAN fct cap
--   -> m (Either UCAN.Error (UCAN fct cap))
-- check receiverDID rawContent ucan = do
--   now <- currentTime
--   case checkTime now ucan of
--     Left err ->
--       return $ Left err

--     Right _  ->
--       case checkReceiver receiverDID ucan of
--         Left  err -> return $ Left err
--         Right _   -> check' rawContent ucan now

-- check' ::
--   ( Proof.Resolver m
--   , FromJSON fct
--   , FromJSON cap
--   , UCAN.Proof.DelegationSemantics cap
--   )
--   => UCAN fct cap
--   -> UTCTime
--   -> m (Either UCAN.Error (UCAN fct cap))
-- check' raw ucan now =
--   case pureChecks raw ucan of
--     Left  err -> return $ Left err
--     Right _   -> checkProof now ucan

pureChecks :: UCAN fct cap -> Either UCAN.Error (UCAN fct cap)
pureChecks ucan = do
  _ <- checkVersion ucan
  checkSignature ucan

checkReceiver :: DID -> UCAN fct cap -> Either UCAN.Error (UCAN fct cap)
checkReceiver recipientDID ucan@UCAN {claims = UCAN.Claims {receiver}} = do
  if receiver == recipientDID
    then Right ucan
    else Left $ ClaimsError IncorrectReceiver

checkVersion :: UCAN fct cap -> Either UCAN.Error (UCAN fct cap)
checkVersion ucan@UCAN { header = UCAN.Header {ucv} } =
  if isSupportedVersion ucv
    then Right ucan
    else Left $ UCAN.HeaderError UnsupportedVersion

-- checkProof ::
--   ( Proof.Resolver m
--   , FromJSON fct
--   , FromJSON rsc
--   , FromJSON ptc
--   , UCAN.Proof.DelegationSemantics rsc
--   , UCAN.Proof.DelegationSemantics ptc
--   )
--   => UTCTime -> UCAN fct rsc ptc -> m (Either UCAN.Error (UCAN fct rsc ptc))
-- checkProof now ucan@UCAN {claims = Claims {proofs}} =
--   case proof of
--     Reference cid ->
--       Proof.resolve cid >>= \case
--         Left err ->
--           return . Left . UCAN.ClaimsError . ProofError . UCAN.Proof.ResolverError $ err

--         Right rawProof -> do
--           case eitherDecode $ decodeUtf8 rawProof of
--             Left _          -> return $ Left UCAN.ParseError
--             Right proofUCAN ->
--               check' rawProof proofUCAN now <&> \case
--                 Left err -> Left err
--                 Right _  -> checkDelegate proofUCAN

--     Nested rawProof proofUCAN ->
--       check' rawProof proofUCAN now <&> \case
--         Left err -> Left err
--         Right _  -> checkDelegate proofUCAN

--     where
--       checkDelegate proofUCAN =
--         case UCAN.Proof.delegatedInBounds ucan proofUCAN of
--           Left err -> Left . UCAN.ClaimsError $ ProofError err
--           Right _  -> Right ucan

checkTime :: UTCTime -> UCAN fct cap -> Either UCAN.Error (UCAN fct cap)
checkTime now ucan@UCAN {claims = UCAN.Claims { expiration, notBefore }} =
  if now > expiration
    then Left $ UCAN.ClaimsError Expired
    else case notBefore of
      Nothing  -> Right ucan
      Just nbf ->
        if now < nbf
          then Left $ UCAN.ClaimsError TooEarly
          else Right ucan

checkSignature :: UCAN fct cap -> Either UCAN.Error (UCAN fct cap)
checkSignature ucan@UCAN {..} =
  case (publicKey, signature) of
    (RSAPublicKey pk, Signature.RS256 (RS256.Signature innerSig)) ->
      if Crypto.RSA.PKCS.verify (Just SHA256) pk content innerSig
        then Right ucan
        else Left $ UCAN.SignatureError SignatureDoesNotMatch

    (Ed25519PublicKey pk, Signature.Ed25519 edSig) ->
      if Crypto.Ed25519.verify pk content edSig
        then Right ucan
        else Left $ UCAN.SignatureError SignatureDoesNotMatch

    (_, _) ->
      Left $ UCAN.SignatureError InvalidPublicKey

  where
    content = encodeUtf8 $ unRawContent signedData
    Claims {sender = DID.Key publicKey} = claims
