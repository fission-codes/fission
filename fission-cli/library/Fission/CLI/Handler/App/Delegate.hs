-- | Delegate capabilities to a DID
module Fission.CLI.Handler.App.Delegate (delegate) where

import qualified Crypto.PubKey.Ed25519                            as Ed25519

import           Crypto.Random.Types

import qualified Data.ByteString.Base64                           as Base64
import qualified Data.ByteString.Char8                            as Char8

import           Data.Function
import qualified Data.Yaml                                        as YAML

import qualified RIO.Text                                         as Text
import qualified RIO.Map                                          as Map

import qualified System.Console.ANSI                              as ANSI
import qualified System.Environment                               as Env

import           Fission.Prelude

import           Fission.Authorization.ServerDID

import           Fission.CLI.Display.Text
import qualified Fission.CLI.Display.Error                        as CLI.Error
import qualified Fission.CLI.Display.Success                      as CLI.Success
import           Fission.CLI.Environment
import           Fission.CLI.Key.Store                            as Key.Store
import           Fission.CLI.Parser.Quiet.Types
import           Fission.CLI.Remote                               as Remote
import           Fission.CLI.WebNative.Mutation.Auth.Store

import           Fission.Error

import           Fission.Internal.UTF8                            as UTF8

import qualified Fission.Key                                      as Key

import           Fission.Web.Auth.Token.Types
import qualified Fission.Web.Auth.Token.UCAN                      as UCAN
import           Fission.Web.Auth.Token.UCAN.Types
import           Fission.Web.Auth.Token.UCAN.Fact.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Potency.Types
import           Fission.Web.API.App.Index.Payload.Types
import           Fission.Web.Client

import           Fission.URL

import           Web.DID.Types                                    as DID

import qualified Web.UCAN                                         as UCAN
import qualified Web.UCAN.Internal.Base64.Scrubbed                as B64
import           Web.UCAN.Internal.Base64.URL                     as B64
import           Web.UCAN.Resolver                                as UCAN.Resolver
import qualified Web.UCAN.Types                                   as UCAN.Types
import           Web.UCAN.Proof                                   as UCAN.Proof
import qualified Web.UCAN.RawContent                              as UCAN.RawContent
import           Web.UCAN.Validation                              (check)


-- | Delegate capabilities to a DID
delegate ::
  ( MonadIO          m
  , MonadLogger      m 
  , MonadRandom      m
  , MonadTime        m

  , MonadEnvironment m
  , MonadRemote      m
  , MonadStore       m
  , MonadWebClient   m
  , ServerDID        m

  , MonadCleanup     m
  , m `Raises` ClientError
  , m `Raises` Key.Error
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound Ed25519.SecretKey
  , m `Raises` NotFound FilePath
  , m `Raises` NotFound UCAN
  , m `Raises` NotFound URL
  , m `Raises` ParseError DID
  , m `Raises` ParseError Potency
  , m `Raises` ParseError UCAN
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` UCAN.Proof.Error
  , m `Raises` UCAN.Error

  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  , UCAN.Resolver.Resolver m

  , Contains (Errors m) (Errors m)
  , Show     (OpenUnion (Errors m))
  )
  => Text
  -> Either String Potency
  -> Either String DID
  -> Int
  -> QuietFlag
  -> m ()
delegate appName potency audienceDid lifetimeInSeconds (QuietFlag quiet) = do
    logDebug @Text "delegate app"

    case audienceDid of
      Left err -> do
        CLI.Error.put err "Could not parse DID"
        raise $ ParseError @DID

      Right did -> do
        remoteUrl <- getRemoteURL
        logDebug $ "Remote URL: " <> show remoteUrl

        let
          URL { domainName } = remoteUrl
          url = URL { domainName, subdomain = Just $ Subdomain appName}
          appResource = Subset $ FissionApp (Subset url)

        ptc <- case potency of
          Left err -> do
            CLI.Error.put err "Could not parse potency. Potency must be AppendOnly, Destructive, or Super_User."
            raise $ ParseError @Potency

          Right ptc ->
            return ptc

        attempt (getCredentialsFor appName appResource ptc) >>= \case
          Left err ->
            raise err

          Right (signingKey, proof) -> do
            now <- getCurrentTime

            let 
              ucan = delegateApp appResource ptc did signingKey proof lifetimeInSeconds now
              encodedUcan = encodeUcan ucan

            if quiet then
              UTF8.putTextLn $ textDisplay encodedUcan

            else do
              CLI.Success.putOk $ "Delegated a UCAN for " <> appName <> " to " <> textDisplay did
              UTF8.putText "🎫 UCAN: "
              colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] do
                UTF8.putTextLn $ textDisplay encodedUcan


getCredentialsFor ::
  ( MonadIO          m
  , MonadLogger      m 
  , MonadRandom      m
  , MonadTime        m

  , MonadEnvironment m
  , MonadStore       m
  , MonadWebClient   m
  , ServerDID        m

  , MonadCleanup     m
  , m `Raises` ClientError
  , m `Raises` Key.Error
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound Ed25519.SecretKey
  , m `Raises` NotFound FilePath
  , m `Raises` NotFound UCAN
  , m `Raises` NotFound URL
  , m `Raises` ParseError UCAN
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` UCAN.Proof.Error
  , m `Raises` UCAN.Error

  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  , UCAN.Resolver.Resolver m

  , Contains (Errors m) (Errors m)
  , Show     (OpenUnion (Errors m))
  ) 
  => Text 
  -> Scope Resource
  -> Potency
  -> m (SecretKey SigningKey, Proof)
getCredentialsFor appName appResource potency = do
  maySigningKey <- liftIO $ Env.lookupEnv "FISSION_MACHINE_KEY"
  mayAppUcan    <- liftIO $ Env.lookupEnv "FISSION_APP_UCAN"

  logDebug $ "FISSION_MACHINE_KEY: " <> show maySigningKey
  logDebug $ "FISSION_APP_UCAN: " <> show mayAppUcan

  case (maySigningKey, mayAppUcan) of
    (Just key, Just appUcan) -> do
      let rawKey = B64.scrub $ Base64.decodeLenient $ Char8.pack key
      signingKey <- ensureM $ Key.Store.parse (Proxy @SigningKey) rawKey
      let did =  DID.Key $ Key.Ed25519PublicKey $ Ed25519.toPublic signingKey

      attempt (checkProofEnvVar appUcan did appResource potency) >>= \case
        Left err -> do
          raise err

        Right ucan -> do
          let
              tokenBS = Char8.pack $ wrapIn "\"" appUcan
              rawContent = UCAN.RawContent.contentOf (decodeUtf8Lenient tokenBS)
              proof = UCAN.Types.Nested rawContent ucan

          logDebug $ "Delegating with FISSION_APP_UCAN: " <> show ucan
          return (signingKey, proof)

    (Just _, Nothing) -> do
      CLI.Error.put
        (Text.pack "Not Found")
        "FISSION_APP_UCAN must be set when delegating with environment variables."
      raise $ NotFound @UCAN

    (Nothing, Just _) -> do
      CLI.Error.put
        (Text.pack "Not Found")
        "FISSION_MACHINE_KEY must be set when delegating with environment variables."
      raise $ NotFound @Ed25519.SecretKey

    (Nothing, Nothing) -> do
      attempt (Key.Store.fetch $ Proxy @SigningKey) >>= \case
        Left err -> do
          CLI.Error.put err $
            "User credentials not found. Please setup an account with `fission setup` or " <>
            "set FISSION_MACHINE_KEY and FISSION_APP_UCAN environment variables."
          raise err

        Right signingKey -> do
          let did =  DID.Key $ Key.Ed25519PublicKey $ Ed25519.toPublic signingKey
          proof <- getRootUserProof

          attempt (checkProofConfig proof did appName appResource potency) >>= \case
            Left err ->
              raise err

            Right prf -> do
              return (signingKey, prf)


checkProofEnvVar ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadRescue      m
  , MonadTime        m

  , m `Raises` ParseError UCAN
  , m `Raises` UCAN.Proof.Error
  , m `Raises` UCAN.Error

  , UCAN.Resolver.Resolver m
  )
  => String
  -> DID
  -> Scope Resource
  -> Potency
  -> m UCAN 
checkProofEnvVar token did resource potency = do
  let tokenBS = Char8.pack $ wrapIn "\"" token

  case UCAN.parse tokenBS of
    Left err -> do
      CLI.Error.put err "Unable to parse UCAN set in FISSION_APP_UCAN environment variable"
      raise $ ParseError @UCAN
          
    Right ucan -> do
      let rawContent = UCAN.RawContent.contentOf (decodeUtf8Lenient tokenBS)
      capableUcan <- ensureM $ checkCapability resource potency ucan
      ensureM $ check did rawContent capableUcan


checkProofConfig ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadTime        m
  , MonadWebClient   m
  , ServerDID        m

  , MonadCleanup     m
  , m `Raises` ClientError
  , m `Raises` NotFound URL
  , m `Raises` UCAN.Error
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` UCAN.Proof.Error

  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  , UCAN.Resolver.Resolver m

  , Contains (Errors m) (Errors m)
  , Show     (OpenUnion (Errors m))
  )
  => Proof
  -> DID
  -> Text
  -> Scope Resource
  -> Potency
  -> m Proof 
checkProofConfig proof did appName appResource potency = do
  case proof of
    UCAN.Types.RootCredential -> do
      ensureM $ checkAppRegistration appName proof
      return proof

    UCAN.Types.Nested rawContent ucan -> do
      capableUcan <- ensureM $ checkCapability appResource potency ucan
      ensureM $ check did rawContent capableUcan
      return proof

    UCAN.Types.Reference cid -> do
      let
        parseToken :: ByteString -> Either UCAN.Resolver.Error (UCAN.Types.UCAN Fact (Scope Resource) Potency)
        parseToken bs =  UCAN.parse bs

      proofTokenBS <- ensureM $ UCAN.Resolver.resolve cid
      let rawContent = UCAN.RawContent.contentOf (decodeUtf8Lenient proofTokenBS)
      ucan <- ensure $ parseToken proofTokenBS

      capableUcan <- ensureM $ checkCapability appResource potency ucan
      ensureM $ check did rawContent capableUcan 
      return proof


checkCapability ::
  ( MonadIO          m
  , MonadLogger      m
  )
  => Scope Resource
  -> Potency
  -> UCAN
  -> m (Either UCAN.Proof.Error UCAN)
checkCapability requestedResource requestedPotency ucan = do
  let
    UCAN.Types.UCAN {claims = UCAN.Types.Claims {resource = mayResource, potency = mayPotency}} = ucan
    putPotencyError = CLI.Error.put PotencyEscelation "UCAN does not have sufficient potency to delegate"
    putScopeError = CLI.Error.put ScopeOutOfBounds "UCAN does not have sufficient scope to delegate"

  case mayResource of
    Just rsc ->
      if rsc `canDelegate` requestedResource then
        case mayPotency of
          Just ptc ->
            if  ptc `canDelegate` requestedPotency then
              return $ Right ucan

            else do
              putPotencyError
              return $ Left PotencyEscelation

          Nothing -> do
            putPotencyError
            return $ Left PotencyEscelation

      else do
        putScopeError
        return $ Left ScopeOutOfBounds

    Nothing -> do
      putScopeError
      return $ Left ScopeOutOfBounds


checkAppRegistration :: 
  ( MonadIO          m
  , MonadLogger      m 
  , MonadTime        m
  , MonadWebClient   m
  , ServerDID        m

  , MonadCleanup     m
  , m `Raises` ClientError
  , m `Raises` NotFound URL

  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey

  , Contains (Errors m) (Errors m)
  , Show     (OpenUnion (Errors m))
  )
  => Text 
  -> Proof 
  -> m (Either (ErrorCase m) Bool)
checkAppRegistration appName proof = do
  attempt (sendAuthedRequest proof appIndex) >>= \case
    Left err -> do
      CLI.Error.put err "Unable to retrieve a list of your apps from the server."
      raise err

    Right index -> do
      let
        registered =
          Map.elems index
            & concatMap (\Payload { urls } -> urls)
            & fmap (fst . Text.break (== '.') . textDisplay)
            & elem appName

      if registered then
        return $ Right True

      else do
        CLI.Error.put (Text.pack "Not found") $
          "Unable to find an app named " <> appName <>
          ". Is the name right and have you registered it?"
        raise $ NotFound @URL



delegateApp :: Scope Resource -> Potency -> DID -> Ed25519.SecretKey -> Proof -> Int -> UTCTime -> UCAN
delegateApp resource potency targetDID sk proof lifetime now =
  UCAN.mkUCAN targetDID sk start expire [] (Just resource) (Just potency) proof
    where
      start  = addUTCTime (secondsToNominalDiffTime (-30))                    now
      expire = addUTCTime (secondsToNominalDiffTime (fromIntegral lifetime))  now


encodeUcan :: UCAN -> Text
encodeUcan UCAN.Types.UCAN {..} =
  let
    rawContent = B64.encodeJWT header claims
  in
  textDisplay rawContent <> "." <> textDisplay sig
