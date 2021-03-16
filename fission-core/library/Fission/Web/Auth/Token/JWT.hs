module Fission.Web.Auth.Token.JWT
  ( getRoot
  , getRootDID
  , mkUCAN
  , delegateSuperUser
  , delegateAppendAll
  , simpleWNFS
  , prettyPrintGrants
  , module Fission.Web.Auth.Token.JWT.Types
  , module Fission.Web.Auth.Token.JWT.Error
  ) where

import qualified Crypto.PubKey.Ed25519                            as Ed25519
import qualified RIO.Text                                         as Text

import           Fission.Prelude

import qualified Fission.Key                                      as Key
import           Fission.User.DID                                 as DID

import           Fission.Authorization                            as Authorization
import           Fission.Key.Asymmetric.Algorithm.Types           as Key

import qualified Fission.Key.Asymmetric.Public.Types              as Asymmetric

import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Web.Auth.Token.JWT.Fact.Types
import qualified Fission.Web.Auth.Token.JWT.Header.Typ.Types      as JWT.Typ
import qualified Fission.Web.Auth.Token.JWT.Resolver              as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class        as Proof
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error        as Resolver
import           Fission.Web.Auth.Token.JWT.Types                 as JWT
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

-- Reexports

import           Fission.Web.Auth.Token.JWT.Error
import           Fission.Web.Auth.Token.JWT.Types

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
