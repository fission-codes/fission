module Fission.Web.Auth.Token.UCAN
  ( getRoot
  , getRootDID
  , mkUCAN
  , delegateSuperUser
  , delegateAppendAll
  , simpleWNFS
  , proveWNFS
  , prettyPrintGrants
  , module Fission.Web.Auth.Token.UCAN.Fact.Types
  , module Fission.Web.Auth.Token.UCAN.Resource.Types
  , module Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
  , module Web.UCAN.Error
  , module Web.UCAN.RawContent
  ) where

import qualified Crypto.PubKey.Ed25519                            as Ed25519
import qualified RIO.Text                                         as Text

import           Fission.Prelude

import qualified Fission.Key                                      as Key

import           Fission.Authorization                            as Authorization

import           Crypto.Key.Asymmetric.Algorithm.Types            as Key
import qualified Crypto.Key.Asymmetric.Public.Types               as Asymmetric

import           Web.DID.Types                                    as DID
import qualified Web.UCAN                                         as UCAN
import qualified Web.UCAN.Header.Typ.Types                        as UCAN.Typ
import qualified Web.UCAN.Resolver                                as UCAN
import qualified Web.UCAN.Resolver.Class                          as Proof
import qualified Web.UCAN.Resolver.Error                          as Resolver
import           Web.UCAN.Types                                   as UCAN

import           Fission.Web.Auth.Token.UCAN.Fact.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types
import qualified Fission.Web.Auth.Token.UCAN.Types                as Fission

-- Reexports

import           Web.UCAN.Error
import           Web.UCAN.RawContent

getRoot :: UCAN.Resolver m => Fission.UCAN -> m (Either Resolver.Error Fission.UCAN)
getRoot ucan@UCAN {claims = Claims {proof}} =
  case proof of
    RootCredential ->
      return $ Right ucan

    Nested _ proofUCAN ->
      getRoot proofUCAN

    Reference cid -> do
      resolved <- Proof.resolve cid
      case resolved of
        Left err -> return $ Left err
        Right rawContent ->
          case UCAN.fromRawContent rawContent of
            Left err -> return $ Left err
            Right proofUCAN ->
              getRoot proofUCAN

getRootDID ::
  ( UCAN.Resolver m
  , MonadRaise    m
  , m `Raises` Resolver.Error
  )
  => Asymmetric.Public
  -> Fission.Proof
  -> m DID
getRootDID fallbackPK = \case
  RootCredential ->
    return $ DID Key fallbackPK

  Nested _ ucan ->  do
    UCAN {claims = Claims {sender}} <- ensureM $ getRoot ucan
    return sender

  Reference cid -> do
    rawContent <- ensureM $ Proof.resolve cid
    case UCAN.fromRawContent rawContent of
      Left err -> raise err
      Right UCAN {claims = Claims {proof}} ->
        getRootDID fallbackPK proof

simpleWNFS :: UTCTime -> DID -> Ed25519.SecretKey -> [Fact] -> Fission.Proof -> Fission.UCAN
simpleWNFS now receiverDID sk facts proof =
  mkUCAN receiverDID sk begin expiry facts resource potency proof
  where
    potency  = Just AppendOnly
    resource = Just (Subset (FissionFileSystem "/"))

    -- Accounting for minor clock drift
    begin  = addUTCTime (secondsToNominalDiffTime (-30)) now
    expiry = addUTCTime (secondsToNominalDiffTime   30)  now

proveWNFS :: UTCTime -> DID -> Ed25519.SecretKey -> [Fact] -> Fission.Proof -> Fission.UCAN
proveWNFS now receiverDID sk facts proof =
  mkUCAN receiverDID sk begin expiry facts resource potency proof
  where
    potency  = Nothing
    resource = Nothing

    -- Accounting for minor clock drift
    begin  = addUTCTime (secondsToNominalDiffTime (-30)) now
    expiry = addUTCTime (secondsToNominalDiffTime   30)  now

delegateAppendAll :: DID -> Ed25519.SecretKey -> Fission.Proof -> UTCTime -> Fission.UCAN
delegateAppendAll targetDID sk proof now =
  mkUCAN targetDID sk start expire [] (Just Complete) (Just AppendOnly) proof
  where
    start  = addUTCTime (secondsToNominalDiffTime (-30)) now
    expire = addUTCTime (nominalDay * 365 * 255)         now

delegateSuperUser :: DID -> Ed25519.SecretKey -> Fission.Proof -> UTCTime -> Fission.UCAN
delegateSuperUser targetDID sk proof now =
  mkUCAN targetDID sk start expire [] (Just Complete) (Just SuperUser) proof
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
  -> Maybe Potency
  -> Fission.Proof
  -> Fission.UCAN
mkUCAN receiver senderSK nbf exp facts resource potency proof = UCAN {..}
  where
    sig = signEd25519 header claims senderSK

    sender = DID
      { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic senderSK
      , method    = DID.Key
      }

    claims = Claims {..}

    header = Header
      { typ = UCAN.Typ.JWT
      , alg = Key.Ed25519
      , cty = Nothing
      , uav = Authorization.latestVersion
      }

prettyPrintGrants :: Fission.UCAN -> Text
prettyPrintGrants UCAN {claims = Claims {..}} =
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
