module Fission.Web.Auth.Token.JWT
  ( getRoot
  , getRootDID
  , mkUCAN
  , delegateSuperUser
  , delegateAppendAll
  , simpleWNFS
  , proveWNFS
  , prettyPrintGrants
  , module Web.JWT.Types
  , module Web.JWT.Error
  ) where

import qualified Crypto.PubKey.Ed25519                            as Ed25519
import qualified RIO.Text                                         as Text

import           Fission.Prelude

import qualified Fission.Key                                      as Key

import           Fission.Authorization                            as Authorization

import           Crypto.Key.Asymmetric.Algorithm.Types            as Key
import qualified Crypto.Key.Asymmetric.Public.Types               as Asymmetric

import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Web.Auth.Token.JWT.Fact.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import           Web.DID.Types                                    as DID
import qualified Web.JWT.Header.Typ.Types                         as JWT.Typ
import qualified Web.JWT.Resolver                                 as JWT
import qualified Web.JWT.Resolver.Class                           as Proof
import qualified Web.JWT.Resolver.Error                           as Resolver
import           Web.JWT.Types                                    as JWT

-- Reexports

import           Web.JWT.Error
import           Web.JWT.Types

getRoot :: JWT.Resolver m => JWT -> m (Either Resolver.Error JWT)
getRoot jwt@JWT {claims = Claims {proof}} =
  case proof of
    RootCredential ->
      return $ Right jwt

    Nested _ proofJWT ->
      getRoot proofJWT

    Reference cid ->
      Proof.resolve cid >>= \case
        Right (_, proofJWT) -> getRoot proofJWT
        Left err            -> return $ Left err

getRootDID ::
  ( JWT.Resolver m
  , MonadRaise   m
  , m `Raises` Resolver.Error
  )
  => Asymmetric.Public
  -> Proof
  -> m DID
getRootDID fallbackPK = \case
  RootCredential ->
    return $ DID Key fallbackPK

  Nested _ jwt ->  do
    JWT {claims = JWT.Claims {sender}} <- ensureM $ getRoot jwt
    return sender

  Reference cid -> do
    (_, JWT {claims = JWT.Claims {proof}}) <- ensureM $ Proof.resolve cid
    getRootDID fallbackPK proof

simpleWNFS :: UTCTime -> DID -> Ed25519.SecretKey -> [Fact] -> Proof -> JWT
simpleWNFS now receiverDID sk facts proof =
  mkUCAN receiverDID sk begin expiry facts resource potency proof
  where
    potency  = AppendOnly
    resource = Just (Subset (FissionFileSystem "/"))

    -- Accounting for minor clock drift
    begin  = addUTCTime (secondsToNominalDiffTime (-30)) now
    expiry = addUTCTime (secondsToNominalDiffTime   30)  now

proveWNFS :: UTCTime -> DID -> Ed25519.SecretKey -> [Fact] -> Proof -> JWT
proveWNFS now receiverDID sk facts proof =
  mkUCAN receiverDID sk begin expiry facts resource potency proof
  where
    potency  = AuthNOnly
    resource = Nothing

    -- Accounting for minor clock drift
    begin  = addUTCTime (secondsToNominalDiffTime (-30)) now
    expiry = addUTCTime (secondsToNominalDiffTime   30)  now

delegateAppendAll :: DID -> Ed25519.SecretKey -> Proof -> UTCTime -> JWT
delegateAppendAll targetDID sk proof now =
  mkUCAN targetDID sk start expire [] (Just Complete) AppendOnly proof
  where
    start  = addUTCTime (secondsToNominalDiffTime (-30)) now
    expire = addUTCTime (nominalDay * 365 * 255)         now

delegateSuperUser :: DID -> Ed25519.SecretKey -> Proof -> UTCTime -> JWT
delegateSuperUser targetDID sk proof now =
  mkUCAN targetDID sk start expire [] (Just Complete) SuperUser proof
  where
    start  = addUTCTime (secondsToNominalDiffTime (-30)) now
    expire = addUTCTime (nominalDay * 365 * 255)         now

mkUCAN ::
     DID
  -> Ed25519.SecretKey
  -> UTCTime
  -> UTCTime
  -> [Fact]
  -> Maybe (Scope Resource)
  -> Potency
  -> Proof
  -> JWT
mkUCAN receiver senderSK nbf exp facts resource potency proof = JWT {..}
  where
    sig = signEd25519 header claims senderSK

    sender = DID
      { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic senderSK
      , method    = DID.Key
      }

    claims = JWT.Claims {..}

    header = JWT.Header
      { typ = JWT.Typ.JWT
      , alg = Key.Ed25519
      , cty = Nothing
      , uav = Authorization.latestVersion
      }

prettyPrintGrants :: JWT -> Text
prettyPrintGrants JWT {claims = JWT.Claims {..}} =
  mconcat
    [ textDisplay potency
    , " "
    , humanizedResource
    , ", from "
    , toHumanTime nbf
    , " until "
    , toHumanTime exp
    ]

  where
    toHumanTime utcTime =
      Text.pack $ formatTime defaultTimeLocale rfc822DateFormat utcTime

    humanizedResource =
      case resource of
        Nothing           -> "no resources"
        Just Complete     -> "all resources"
        Just (Subset res) -> textDisplay res
