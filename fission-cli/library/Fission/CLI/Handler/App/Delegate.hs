-- | Delegate capabilities to a key pair or DID
module Fission.CLI.Handler.App.Delegate (delegate) where

import qualified Crypto.PubKey.Ed25519                     as Ed25519

import           Crypto.Random.Types

import qualified Data.ByteString.Base64                    as Base64
import qualified Data.ByteString.Char8                     as Char8

import           Data.Function
import qualified Data.Yaml                                 as YAML
import qualified RIO.Text                                  as Text
import qualified RIO.Map                                   as Map

import qualified System.Environment                        as Env

import           Fission.Prelude

import           Fission.Error
import qualified Fission.Key                                 as Key
import           Fission.URL

import qualified Fission.App.Name                            as App

import           Fission.Authorization.ServerDID

import           Fission.Error.Types

import           Fission.CLI.Key.Store                       as Key.Store
import           Fission.Web.Auth.Token.Types
import           Fission.Web.Auth.Token.UCAN.Types
import           Fission.Web.Client

import qualified Fission.CLI.Display.Error                 as CLI.Error
import qualified Fission.CLI.Display.Success               as CLI.Success
import           Fission.CLI.Key.Ed25519

import           Fission.CLI.Environment
import           Fission.CLI.WebNative.Mutation.Auth.Store

import qualified Fission.Web.Auth.Token.UCAN                      as UCAN
import           Fission.Web.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Potency.Types
import           Fission.Web.API.App.Index.Payload.Types
 

import           Web.DID.Types                              as DID


import qualified Web.UCAN
import qualified Web.UCAN.Internal.Base64                   as B64
import qualified Web.UCAN.Internal.Base64.Scrubbed          as B64.Scrubbed

import           Web.UCAN.Internal.Base64.URL               as B64.URL
import           Web.UCAN.Resolver.Error                    as UCAN.Resolver
import qualified Web.UCAN.Types                             as UCAN.Types


import           Fission.Internal.UTF8 (wrapIn)

-- | Delegate capabilities to a key pair or DID
delegate ::
  ( MonadIO          m
  , MonadLogger      m 
  , MonadRandom m
  , MonadTime        m

  , MonadEnvironment m
  , MonadStore  m
  , MonadWebClient   m
  , ServerDID        m

  , MonadCleanup     m
  , m `Raises` ClientError
  , m `Raises` Key.Error
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  , m `Raises` ParseError DID
  , m `Raises` UCAN.Resolver.Error

  , Contains (Errors m) (Errors m)
  , Display  (OpenUnion (Errors m))
  , Show     (OpenUnion (Errors m))

  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => Text
  -> Either String DID
  -> Int
  -> m ()
delegate appName audienceDid lifetimeInSeconds = do
    logDebug @Text "delegate"

    logDebug $ "App Name: " <> show audienceDid
    logDebug $ "Raw Audience DID: " <> show audienceDid
    logDebug $ "Lifetime: " <> show lifetimeInSeconds

    maySigningKey <- liftIO $ Env.lookupEnv "FISSION_MACHINE_KEY"
    mayAppUcan <- liftIO $ Env.lookupEnv "FISSION_APP_UCAN"

    logError $ "FISSION_MACHINE_KEY: " <> show maySigningKey
    logError $ "FISSION_APP_UCAN: " <> show mayAppUcan

    (signingKey, proof) <- case (maySigningKey, mayAppUcan) of
      (Just key, Just appUcan) -> do
        -- Sign with key, use appUcan as proof
        let
          rawKey = B64.Scrubbed.scrub $ Base64.decodeLenient $ Char8.pack key
          -- maybeUcan = checkProofToken appUcan

        attempt (checkProofToken appUcan) >>= \case
          Left err -> do
             logError $ "UCAN Validation error: " <> show err
             raise err

          Right ucan -> do
            logDebug $ "UCAN Result: " <> show ucan

            signingKey <- ensureM $ Key.Store.parse (Proxy @SigningKey) rawKey

            -- Convert ucan to proof and use below
            return (signingKey, UCAN.Types.RootCredential)

      (Just key, Nothing) -> do
        -- Sign with key, assume key has root authority

        let
          rawKey = B64.Scrubbed.scrub $ Base64.decodeLenient $ Char8.pack key

        signingKey <- ensureM $ Key.Store.parse (Proxy @SigningKey) rawKey
        return (signingKey, UCAN.Types.RootCredential)

      (Nothing, _) -> do
        -- Use normal CLI config from keystore
        signingKey <- Key.Store.fetch $ Proxy @SigningKey
        proof <- getRootUserProof

        logDebug $ "Proof from config: " <> textDisplay proof

        return (signingKey, proof)

    appRegistered <- checkAppRegistration appName proof

    if appRegistered then do
      logDebug $ appName <> " is registered!"

      case audienceDid of
        Left err -> do
          CLI.Error.put err "Could not parse DID"
          raise $ ParseError @DID

        Right did -> do
          logDebug $ "Audience DID: " <> textDisplay did

          now <- getCurrentTime

          let 
            ucan = delegateAppendApp appName did signingKey proof now
            encodedUcan = encodeUcan ucan

          logDebug $ "UCAN " <> textDisplay encodedUcan 

    else
      logDebug $ appName <> " is not registered."


checkProofToken ::
  ( MonadRaise m
  , m `Raises` UCAN.Resolver.Error
  )
  => String
  -> m UCAN
checkProofToken token = do
  let
    tokenBS = Char8.pack $ wrapIn "\"" token

  case Web.UCAN.parse tokenBS of
    Left err ->
      raise err
          
    Right ucan ->
      -- Do more to check and return a UCAN.Proof
      return ucan


checkAppRegistration :: 
  ( MonadIO          m
  , MonadLogger      m 
  , MonadTime        m

  , MonadWebClient   m
  , ServerDID        m

  , MonadCleanup     m
  , m `Raises` ClientError

  , Contains (Errors m) (Errors m)
  , Display  (OpenUnion (Errors m))
  , Show     (OpenUnion (Errors m))

  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => Text 
  -> Proof 
  -> m Bool 
checkAppRegistration appName proof = do
    attempt (sendAuthedRequest proof appIndex) >>= \case
      Left err -> do
        logDebug $ textDisplay err
        CLI.Error.put err $ textDisplay err
        raise err

      Right index -> do
        let
          registered = 
            Map.elems index
              & concatMap (\Payload { urls } -> urls)
              & fmap (fst . Text.break (== '.') . textDisplay)
              & elem appName

        return registered


delegateAppendApp :: Text -> DID -> Ed25519.SecretKey -> Proof -> UTCTime -> UCAN
delegateAppendApp appName targetDID sk proof now =
  let
    url = URL { domainName = "fission.app", subdomain = Just $ Subdomain appName}
    resource = FissionApp (Subset url)
    scope = Subset resource

  in
  UCAN.mkUCAN targetDID sk start expire [] (Just scope) (Just AppendOnly) proof
    where
      start  = addUTCTime (secondsToNominalDiffTime (-30)) now
      expire = addUTCTime (nominalDay * 365)         now


encodeUcan :: UCAN -> Text
encodeUcan UCAN.Types.UCAN {..} =
  let
    rawContent = UCAN.RawContent $ B64.URL.encodeJWT header claims
  in
  textDisplay rawContent <> "." <> textDisplay sig
