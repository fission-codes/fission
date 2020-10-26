module Fission.Web.Auth.Token.JWT.Validation
  ( check
  , pureChecks
  , checkTime
  , checkReceiver
  , checkSignature
  , checkEd25519Signature
  , checkRSA2048Signature
  ) where

import qualified RIO.ByteString                                   as BS
import qualified RIO.Text                                         as Text

import qualified Data.Bits                                        as Bits

import           Fission.Web.Auth.Token.UCAN.Privilege.Types

import qualified Fission.Web.Auth.Token.JWT.RawContent.Types      as JWT

import qualified Fission.Web.Auth.Token.UCAN.Types                as UCAN

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT.Proof.Error

import           Fission.Web.Auth.Token.UCAN.Attenuated.Types
import           Fission.Web.Auth.Token.UCAN.Proof.Types

-- FIXME WNFS.Types would be nice
import qualified Fission.WNFS.Privilege.Types                     as WNFS
import           Fission.WNFS.Subgraph.Types                      as WNFS

import qualified Fission.Web.Auth.Token.UCAN.Privilege.Types      as Privilege



import           Network.IPFS.CID.Types



import           Crypto.Hash.Algorithms                           (SHA256 (..))
import qualified Crypto.PubKey.Ed25519                            as Crypto.Ed25519
import qualified Crypto.PubKey.RSA.PKCS15                         as Crypto.RSA.PKCS

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

import           Fission.Web.Auth.Token.JWT.Error                 as JWT

import           Fission.Web.Auth.Token.UCAN.Types

check ::
  ( m `Proof.Resolves` UCAN privilege fact
  , MonadTime m
  )
  => JWT.RawContent
  -> UCAN privilege fact
  -> m (Either JWT.Error (UCAN privilege fact))
check rawContent jwt = pureChecks rawContent jwt <$> currentTime

pureChecks ::
     JWT.RawContent
  -> UCAN privilege fact
  -> UTCTime
  -> Either JWT.Error (UCAN privilege fact)
pureChecks raw jwt now = do
  _ <- checkVersion  jwt
  _ <- checkTime now jwt
  checkSignature raw jwt

checkReceiver :: -- FIXME run on the first level UCAN before this kickoff
  ServerDID m
  => UCAN privilege fact
  -> m (Either JWT.Error (UCAN privilege fact))
checkReceiver ucan@UCAN {claims = UCAN.Claims {receiver}} = do
  serverDID <- getServerDID
  return if receiver == serverDID
    then Right ucan
    else Left $ ClaimsError IncorrectReceiver

checkVersion :: UCAN privilege fact -> Either JWT.Error (UCAN privilege fact)
checkVersion ucan@UCAN { header = UCAN.Header {ucv = SemVer mjr mnr pch}} =
  if mjr == 0 && mnr >=4 && pch >= 0
    then Right ucan
    else Left $ JWT.HeaderError UnsupportedVersion

checkTime ::
     UTCTime
  -> UCAN privilege fact
  -> Either JWT.Error (UCAN privilege fact)
checkTime now ucan@UCAN {claims = UCAN.Claims { exp, nbf }} = do
  if | now > exp -> Left $ JWT.ClaimsError Expired
     | now < nbf -> Left $ JWT.ClaimsError TooEarly
     | otherwise -> Right ucan

checkSignature ::
     JWT.RawContent
  -> UCAN privilege fact
  -> Either JWT.Error (UCAN privilege fact)
checkSignature rawContent ucan@UCAN {sig} =
  case sig of
    Signature.Ed25519 _        -> checkEd25519Signature rawContent ucan
    Signature.RS256   rs256Sig -> checkRSA2048Signature rawContent ucan rs256Sig

checkRSA2048Signature ::
     JWT.RawContent
  -> UCAN privilege fact
  -> RS256.Signature
  -> Either JWT.Error (UCAN privilege fact)
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

checkEd25519Signature ::
     JWT.RawContent
  -> UCAN privilege fact
  -> Either JWT.Error (UCAN privilege fact)
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

signaturesMatch ::
     UCAN privilege fact
  -> UCAN privilege fact
  -> Either JWT.Error (UCAN privilege fact)
signaturesMatch jwt prfJWT =
  if (jwt |> claims |> sender) == (prfJWT |> claims |> receiver)
    then Right jwt
    else Left . ClaimsError $ ProofError InvalidSignatureChain

timeInSubset ::
     UCAN privilege fact
  -> UCAN privilege fact
  -> Either JWT.Error (UCAN privilege fact)
timeInSubset jwt prfJWT =
  if startBoundry && expiryBoundry
    then Right jwt
    else Left . ClaimsError $ ProofError TimeNotSubset

  where
    startBoundry  = (jwt |> claims |> nbf) >= (prfJWT |> claims |> nbf)
    expiryBoundry = (jwt |> claims |> exp) <= (prfJWT |> claims |> exp)
