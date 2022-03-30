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

import           Web.UCAN.Resolver              as Witness

import           Web.UCAN.Claims.Error
import           Web.UCAN.Header.Error
import           Web.UCAN.Signature.Error

import           Web.UCAN                       as UCAN
import           Web.UCAN.Error                 as UCAN
import           Web.UCAN.Witness               as UCAN.Witness

import           Web.UCAN.Header
import qualified Web.UCAN.Signature.RS256.Types as RS256
import           Web.UCAN.Signature.Types       as Signature


-- check ::
--   ( Witness.Resolver m
--   , MonadTime      m
--   , FromJSON fct
--   , FromJSON cap
--   , UCAN.Witness.DelegationSemantics cap
--   )
--   => DID
--   -> UCAN fct res abl
--   -> m (Either UCAN.Error (UCAN fct res abl))
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
--   ( Witness.Resolver m
--   , FromJSON fct
--   , FromJSON cap
--   , UCAN.Witness.DelegationSemantics cap
--   )
--   => UCAN fct res abl
--   -> UTCTime
--   -> m (Either UCAN.Error (UCAN fct res abl))
-- check' raw ucan now =
--   case pureChecks raw ucan of
--     Left  err -> return $ Left err
--     Right _   -> checkWitness now ucan

pureChecks :: UCAN fct res abl -> Either UCAN.Error (UCAN fct res abl)
pureChecks ucan = do
  _ <- checkVersion ucan
  checkSignature ucan

checkReceiver :: DID -> UCAN fct res abl -> Either UCAN.Error (UCAN fct res abl)
checkReceiver recipientDID ucan@UCAN {claims = UCAN.Claims {receiver}} = do
  if receiver == recipientDID
    then Right ucan
    else Left $ ClaimsError IncorrectReceiver

checkVersion :: UCAN fct res abl -> Either UCAN.Error (UCAN fct res abl)
checkVersion ucan@UCAN { header = UCAN.Header {ucv} } =
  if isSupportedVersion ucv
    then Right ucan
    else Left $ UCAN.HeaderError UnsupportedVersion

-- checkWitness ::
--   ( Witness.Resolver m
--   , FromJSON fct
--   , FromJSON rsc
--   , FromJSON ptc
--   , UCAN.Witness.DelegationSemantics rsc
--   , UCAN.Witness.DelegationSemantics ptc
--   )
--   => UTCTime -> UCAN fct rsc ptc -> m (Either UCAN.Error (UCAN fct rsc ptc))
-- checkWitness now ucan@UCAN {claims = Claims {proofs}} =
--   case proof of
--     Reference cid ->
--       Witness.resolve cid >>= \case
--         Left err ->
--           return . Left . UCAN.ClaimsError . WitnessError . UCAN.Witness.ResolverError $ err

--         Right rawWitness -> do
--           case eitherDecode $ decodeUtf8 rawWitness of
--             Left _          -> return $ Left UCAN.ParseError
--             Right proofUCAN ->
--               check' rawWitness proofUCAN now <&> \case
--                 Left err -> Left err
--                 Right _  -> checkDelegate proofUCAN

--     Nested rawWitness proofUCAN ->
--       check' rawWitness proofUCAN now <&> \case
--         Left err -> Left err
--         Right _  -> checkDelegate proofUCAN

--     where
--       checkDelegate proofUCAN =
--         case UCAN.Witness.delegatedInBounds ucan proofUCAN of
--           Left err -> Left . UCAN.ClaimsError $ WitnessError err
--           Right _  -> Right ucan

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

checkSignature :: UCAN fct res abl -> Either UCAN.Error (UCAN fct res abl)
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
