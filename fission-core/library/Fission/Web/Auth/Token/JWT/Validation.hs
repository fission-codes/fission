module Fission.Web.Auth.Token.JWT.Validation
  ( check
  , check'
  , pureChecks
  , checkTime
  , checkSignature
  , checkEd25519Signature
  , checkRSA2048Signature
  ) where

import qualified RIO.ByteString                                   as BS
import qualified RIO.List                                         as List
import qualified RIO.Text                                         as Text

import qualified Data.Bits                                        as Bits

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT                       as JWT
import           Fission.Web.Auth.Token.JWT.Proof.Error

import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types



import           Fission.URL.Types
import qualified Fission.User.Username.Types                      as Username


import           Network.IPFS.CID.Types



import           Crypto.Hash.Algorithms                           (SHA256 (..))
import qualified Crypto.PubKey.Ed25519                            as Crypto.Ed25519
import qualified Crypto.PubKey.RSA.PKCS15                         as Crypto.RSA.PKCS

import           Fission.Prelude
import           Fission.SemVer.Types

import           Fission.Key                                      as Key
import qualified Fission.User                                     as User

import           Fission.Authorization.ServerDID.Class

-- FIXME move to proof submoule?
import           Fission.Web.Auth.Token.JWT.Proof.Error           as UCAN.Proof
import           Fission.Web.Auth.Token.JWT.Resolver              as Proof

import           Fission.Web.Auth.Token.JWT.Claims.Error
import           Fission.Web.Auth.Token.JWT.Header.Error
import           Fission.Web.Auth.Token.JWT.Signature.Error

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
  -> UCAN
  -> m (Either JWT.Error UCAN)
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
  -> UCAN
  -> UTCTime
  -> m (Either JWT.Error UCAN)
check' raw jwt now =
  case pureChecks raw jwt of
    Left  err -> return $ Left err
    Right _   -> checkProof now jwt

pureChecks ::
     JWT.RawContent
  -> UCAN
  -> Either JWT.Error UCAN
pureChecks raw jwt = do
  _ <- checkVersion  jwt
  checkSignature raw jwt

checkReceiver :: ServerDID m => UCAN -> m (Either JWT.Error UCAN)
checkReceiver ucan@UCAN {claims = JWT.Claims {receiver}} = do
  serverDID <- getServerDID
  return if receiver == serverDID
    then Right ucan
    else Left $ ClaimsError IncorrectReceiver

checkVersion :: UCAN -> Either JWT.Error UCAN
checkVersion ucan@UCAN { header = JWT.Header {ucv = SemVer mjr mnr pch}} =
  if mjr == 0 && mnr >=4 && pch >= 0
    then Right ucan
    else Left $ JWT.HeaderError UnsupportedVersion

checkProof ::
  ( ServerDID      m
  , Proof.Resolver m
  )
  => UTCTime
  -> UCAN
  -> m (Either JWT.Error UCAN) -- FIXME JWT.Error ~> UCAN.Error
checkProof now ucan@UCAN {claims = Claims {proofs}} =
  case proofs of
    RootCredential            -> return $ Right ucan
    DelegatedFrom innerProofs -> foldM folder (Right ucan) innerProofs

  where
    folder ::
      ( Proof.Resolver m
      , ServerDID      m
      )
      => (Either JWT.Error UCAN)
      -> DelegateProof
      -> m (Either JWT.Error UCAN)
    folder (Left err) _ = return $ Left err
    folder (Right _) x  = checkInner x

    checkInner ::
      ( Proof.Resolver m
      , ServerDID      m
      )
      => DelegateProof
      -> m (Either JWT.Error UCAN)
    checkInner = \case
      Reference cid            -> resolveCID cid
      Nested rawProof proofJWT -> checkNested rawProof proofJWT

    checkNested ::
      ( ServerDID      m
      , Proof.Resolver m
      )
      => RawContent
      -> UCAN
      -> m (Either JWT.Error UCAN)
    checkNested rawProof proofJWT =
      check' rawProof proofJWT now >>= \case
        Left err -> return $ Left err
        Right _  -> delegatedInBounds ucan proofJWT

    resolveCID ::
      ( ServerDID      m
      , Proof.Resolver m
      )
      => CID
      -> m (Either JWT.Error UCAN)
    resolveCID cid =
      Proof.resolve cid >>= \case
        Left err ->
          return . Left . JWT.ClaimsError . ProofError $ ResolverError err

        Right (rawProof, proofJWT) ->
          check' rawProof proofJWT now >>= \case
            Left err -> return $ Left err
            Right _  -> delegatedInBounds ucan proofJWT

checkTime :: UTCTime -> UCAN -> Either JWT.Error UCAN
checkTime now ucan@UCAN {claims = JWT.Claims { exp, nbf }} = do
  if | now > exp -> Left $ JWT.ClaimsError Expired
     | now < nbf -> Left $ JWT.ClaimsError TooEarly
     | otherwise -> Right ucan

checkSignature :: JWT.RawContent -> UCAN -> Either JWT.Error UCAN
checkSignature rawContent ucan@UCAN {sig} =
  case sig of
    Signature.Ed25519 _        -> checkEd25519Signature rawContent ucan
    Signature.RS256   rs256Sig -> checkRSA2048Signature rawContent ucan rs256Sig

checkRSA2048Signature ::
     JWT.RawContent
  -> UCAN
  -> RS256.Signature
  -> Either JWT.Error UCAN
checkRSA2048Signature (JWT.RawContent raw) ucan@UCAN {..} (RS256.Signature innerSig) = do
  case publicKey of
    RSAPublicKey pk ->
      if Crypto.RSA.PKCS.verify (Just SHA256) pk content innerSig
        then Right ucan
        else Left $ JWT.SignatureError SignatureDoesNotMatch

    _ ->
      Left $ JWT.SignatureError InvalidPublicKey

  where
    content = encodeUtf8 raw
    Claims {sender = User.DID {publicKey}} = claims

checkEd25519Signature :: JWT.RawContent -> UCAN -> Either JWT.Error UCAN
checkEd25519Signature (JWT.RawContent raw) ucan@UCAN {..} =
  case (publicKey, sig) of
    (Ed25519PublicKey pk, Signature.Ed25519 edSig) ->
      if Crypto.Ed25519.verify pk (encodeUtf8 raw) edSig
        then Right ucan
        else Left $ JWT.SignatureError SignatureDoesNotMatch

    (_, _) ->
      Left $ JWT.SignatureError InvalidPublicKey

  where
    Claims {sender = User.DID {publicKey}} = claims

delegatedInBounds ::
  Proof.Resolver m
  => UCAN
  -> UCAN
  -> m (Either JWT.Error UCAN)
delegatedInBounds ucan prfUCAN =
  case pureChecks' of
    Left err ->
      return $ Left err

    Right UCAN {claims = Claims {attenuations = Complete}} ->
      return $ Right ucan

    Right UCAN {claims = Claims {attenuations = Subset atts, proofs}} ->
      foldM (folder proofs) (Right ucan) atts

  where
    folder ::
      Proof.Resolver m
      => Proof
      -> Either JWT.Error UCAN
      -> Attenuation
      -> m (Either JWT.Error UCAN)
    folder _ (Left err) _ =
      return $ Left err

    folder proofs acc att =
      attenuationInProofs att proofs >>= \case
        Left err -> return $ Left err
        Right _  -> return acc

    pureChecks' = do
      _ <- signaturesMatch ucan prfUCAN
      timeInSubset         ucan prfUCAN

  -- FIXME is this only structural, or arewe going to actually check ownsershipo and stuff heer?
  --   I believe so, in Validation.hs

signaturesMatch :: UCAN -> UCAN -> Either JWT.Error UCAN
signaturesMatch jwt prfJWT =
  if (jwt |> claims |> sender) == (prfJWT |> claims |> receiver)
    then Right jwt
    else Left . ClaimsError $ ProofError InvalidSignatureChain

attenuationInProofs ::
  Proof.Resolver m
  => Attenuation
  -> Proof
  -> m (Either JWT.Error ())
attenuationInProofs att = \case
  RootCredential       -> return $ Right ()
  DelegatedFrom proofs -> foldM folder (Right ()) proofs
  where
    folder ::
      Proof.Resolver m
      => Either JWT.Error ()
      -> DelegateProof
      -> m (Either JWT.Error ())
    folder (Left err) _     = return $ Left err
    folder (Right ()) proof = attenuationInDelegateProof att proof

attenuationInDelegateProof ::
  Proof.Resolver m
  => Attenuation
  -> DelegateProof
  -> m (Either JWT.Error ())
attenuationInDelegateProof att = \case
  Reference _cid -> do
    let resolved = undefined --FIXME resoilved CID & rerun
    attenuationInDelegateProof att resolved

  Nested _ UCAN {claims = Claims {attenuations = proofAttenuations}} ->
    -- FIXME we check up the whole chain elsewhere, yes?
    return $ sequence_ (go att <$> proofAttenuations)

  where
    go :: Attenuation -> Attenuation -> Either JWT.Error ()
    go (FileSystem claimWNFS) (FileSystem proofWNFS) =
      case wnfsAttenuationInSubset claimWNFS proofWNFS of
        Left err -> Left . ClaimsError $ ProofError err
        Right () -> Right ()

timeInSubset :: UCAN -> UCAN -> Either JWT.Error UCAN
timeInSubset jwt prfJWT =
  if startBoundry && expiryBoundry
    then Right jwt
    else Left . ClaimsError $ ProofError TimeNotSubset

  where
    startBoundry  = (jwt |> claims |> nbf) >= (prfJWT |> claims |> nbf)
    expiryBoundry = (jwt |> claims |> exp) <= (prfJWT |> claims |> exp)

wnfsAttenuationInSubset :: WNFSAttenuation -> WNFSAttenuation -> Either UCAN.Proof.Error ()
wnfsAttenuationInSubset subject proof =
  case (resourceCheck, capability subject <= capability proof) of
    (False, _) -> Left ResourceEscelation
    (_, False) -> Left CapabilityEscelation
    _          -> Right ()
  where
    resourceCheck =
      wnfsResourceInSubset (wnfsResource subject) (wnfsResource proof)

wnfsResourceInSubset :: WNFSResource -> WNFSResource -> Bool
wnfsResourceInSubset inner outer =
  namespaceMatches && usernameMatches && filePathSubset

  where
    namespaceMatches = namespace inner == namespace outer
    usernameMatches  = username  inner == username  outer

    filePathSubset   =
      if | "/public/"  `Text.isPrefixOf` innerPath -> outerPath `Text.isPrefixOf` innerPath
         | "/private/" `Text.isPrefixOf` innerPath -> containedInPrivatePath innerPath outerPath
         | otherwise                               -> False

    innerPath = normalizePath . Text.pack $ filePath inner
    outerPath = normalizePath . Text.pack $ filePath outer

    normalizePath raw =
      if "/" `Text.isSuffixOf` raw
        then raw
        else raw <> "/"

containedInPrivatePath :: Text -> Text -> Bool
containedInPrivatePath innerPath outerPath =
  zipWith (Bits..&.) innerWords outerWords == outerWords
  where
    innerWords = BS.unpack $ encodeUtf8 innerPath
    outerWords = BS.unpack $ encodeUtf8 outerPath
