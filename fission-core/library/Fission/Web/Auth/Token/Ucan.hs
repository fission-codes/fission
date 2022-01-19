module Fission.Web.Auth.Token.Ucan
  ( getRoot
  , getRootDID
  , mkUCAN
  , delegateSuperUser
  , delegateAppendAll
  , simpleWNFS
  , proveWNFS
  , prettyPrintGrants
  , module Fission.Web.Auth.Token.Ucan.Fact.Types
  , module Fission.Web.Auth.Token.Ucan.Resource.Types
  , module Fission.Web.Auth.Token.Ucan.Resource.Scope.Types
  , module Web.Ucan.Error
  , module Web.Ucan.RawContent
  ) where

import qualified Crypto.PubKey.Ed25519                            as Ed25519
import qualified RIO.Text                                         as Text

import           Fission.Prelude

import qualified Fission.Key                                      as Key

import           Fission.Authorization                            as Authorization

import           Crypto.Key.Asymmetric.Algorithm.Types            as Key
import qualified Crypto.Key.Asymmetric.Public.Types               as Asymmetric

import           Web.DID.Types                                    as DID
import qualified Web.Ucan.Header.Typ.Types                        as Ucan.Typ
import qualified Web.Ucan.Resolver                                as Ucan
import qualified Web.Ucan.Resolver.Class                          as Proof
import qualified Web.Ucan.Resolver.Error                          as Resolver
import           Web.Ucan.Types                                   as Ucan

import           Fission.Web.Auth.Token.Ucan.Fact.Types
import           Fission.Web.Auth.Token.Ucan.Resource.Scope.Types
import           Fission.Web.Auth.Token.Ucan.Resource.Types
import qualified Fission.Web.Auth.Token.Ucan.Types                as Fission

-- Reexports

import           Web.Ucan.Error
import           Web.Ucan.RawContent

getRoot :: Ucan.Resolver m Fact (Scope Resource) => Fission.Ucan -> m (Either Resolver.Error Fission.Ucan)
getRoot ucan@Ucan {claims = Claims {proof}} =
  case proof of
    RootCredential ->
      return $ Right ucan

    Nested _ proofUcan ->
      getRoot proofUcan

    Reference cid ->
      Proof.resolve cid >>= \case
        Right (_, proofUcan) -> getRoot proofUcan
        Left err             -> return $ Left err

getRootDID ::
  ( Ucan.Resolver m Fact (Scope Resource)
  , MonadRaise   m
  , m `Raises` Resolver.Error
  )
  => Asymmetric.Public
  -> Fission.Proof
  -> m DID
getRootDID fallbackPK = \case
  RootCredential ->
    return $ DID Key fallbackPK

  Nested _ ucan ->  do
    Ucan {claims = Claims {sender}} <- ensureM $ getRoot ucan
    return sender

  Reference cid -> do
    (_, Ucan {claims = Claims {proof}}) <- ensureM $ Proof.resolve cid
    getRootDID fallbackPK proof

simpleWNFS :: UTCTime -> DID -> Ed25519.SecretKey -> [Fact] -> Fission.Proof -> Fission.Ucan
simpleWNFS now receiverDID sk facts proof =
  mkUCAN receiverDID sk begin expiry facts resource potency proof
  where
    potency  = AppendOnly
    resource = Just (Subset (FissionFileSystem "/"))

    -- Accounting for minor clock drift
    begin  = addUTCTime (secondsToNominalDiffTime (-30)) now
    expiry = addUTCTime (secondsToNominalDiffTime   30)  now

proveWNFS :: UTCTime -> DID -> Ed25519.SecretKey -> [Fact] -> Fission.Proof -> Fission.Ucan
proveWNFS now receiverDID sk facts proof =
  mkUCAN receiverDID sk begin expiry facts resource potency proof
  where
    potency  = AuthNOnly
    resource = Nothing

    -- Accounting for minor clock drift
    begin  = addUTCTime (secondsToNominalDiffTime (-30)) now
    expiry = addUTCTime (secondsToNominalDiffTime   30)  now

delegateAppendAll :: DID -> Ed25519.SecretKey -> Fission.Proof -> UTCTime -> Fission.Ucan
delegateAppendAll targetDID sk proof now =
  mkUCAN targetDID sk start expire [] (Just Complete) AppendOnly proof
  where
    start  = addUTCTime (secondsToNominalDiffTime (-30)) now
    expire = addUTCTime (nominalDay * 365 * 255)         now

delegateSuperUser :: DID -> Ed25519.SecretKey -> Fission.Proof -> UTCTime -> Fission.Ucan
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
  -> Fission.Proof
  -> Fission.Ucan
mkUCAN receiver senderSK nbf exp facts resource potency proof = Ucan {..}
  where
    sig = signEd25519 header claims senderSK

    sender = DID
      { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic senderSK
      , method    = DID.Key
      }

    claims = Claims {..}

    header = Header
      { typ = Ucan.Typ.JWT
      , alg = Key.Ed25519
      , cty = Nothing
      , uav = Authorization.latestVersion
      }

prettyPrintGrants :: Fission.Ucan -> Text
prettyPrintGrants Ucan {claims = Claims {..}} =
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
