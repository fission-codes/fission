-- | Delegate capabilities to a key pair or DID
module Fission.CLI.Handler.App.Delegate (delegate) where

import qualified Crypto.PubKey.Ed25519                     as Ed25519

import           Crypto.Random.Types
import           Data.Function
import qualified Data.Yaml                                 as YAML
import qualified RIO.Text                                  as Text
import qualified RIO.Map                                   as Map

import           Fission.Prelude

import qualified Fission.Key                                 as Key

import qualified Fission.App.Name                          as App

import           Fission.Authorization.ServerDID

import           Fission.Error.Types

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Auth.Token.UCAN.Types
import           Fission.Web.Client

import qualified Fission.CLI.Display.Error                 as CLI.Error
import qualified Fission.CLI.Display.Success               as CLI.Success
import           Fission.CLI.Key.Ed25519

import           Fission.CLI.Environment
import           Fission.CLI.WebNative.Mutation.Auth.Store as UCAN

import           Web.DID.Types                             as DID

import Fission.Web.API.App.Index.Payload.Types

-- | Delegate capabilities to a key pair or DID
delegate ::
  ( MonadIO          m
  , MonadLogger      m 
  , MonadRandom m
  , MonadTime        m

  , MonadEnvironment m
  , UCAN.MonadStore  m
  , MonadWebClient   m
  , ServerDID        m

  , MonadCleanup     m
  , m `Raises` ClientError
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath

  , Contains (Errors m) (Errors m)
  , Display  (OpenUnion (Errors m))
  , Show     (OpenUnion (Errors m))

  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => Text
  -> Bool 
  -> Maybe DID
  -> m ()
delegate appName generateKey mayAudienceDid = do
    logDebug @Text "delegate"

    proof <- getRootUserProof
    appRegistered <- checkAppRegistration appName proof

    if appRegistered then do
      logDebug $ appName <> " is registered!"

      case mayAudienceDid of
        Just audienceDid ->
          logDebug $ textDisplay audienceDid <> " is the audience DID"

        Nothing -> do
          logDebug @Text "🏗️  Generating signing key"

          secretKey <- Ed25519.generateSecretKey
          publicKey <- return $ Ed25519.toPublic secretKey
          did <- return $ DID.Key $ Key.Ed25519PublicKey publicKey

          logDebug $ "Secret key " <> show secretKey
          logDebug $ "Public key " <> textDisplay publicKey
          logDebug $ "DID " <> textDisplay did

    else
      logDebug $ appName <> " is not registered."


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