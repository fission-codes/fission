-- | Delegate capabilities to a key pair or DID
module Fission.CLI.Handler.App.Delegate (delegate) where

import qualified Crypto.PubKey.Ed25519                     as Ed25519
import           Data.Function
import qualified Data.Yaml                                 as YAML
import qualified RIO.Text                                  as Text
import qualified RIO.Map                                   as Map
import qualified System.Console.ANSI                       as ANSI

import           Fission.Prelude

import qualified Fission.App.Name                          as App
import           Fission.Authorization.ServerDID
import           Fission.Error.Types
import qualified Fission.Internal.UTF8                     as UTF8

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Auth.Token.UCAN.Types
import           Fission.Web.Client

import           Fission.CLI.Display.Text

import qualified Fission.CLI.Display.Error                 as CLI.Error
import qualified Fission.CLI.Display.Success               as CLI.Success

import qualified Fission.CLI.App.Environment               as App.Env
import qualified Fission.CLI.Prompt.BuildDir               as BuildDir

import           Fission.CLI.Environment
import           Fission.CLI.WebNative.Mutation.Auth.Store as UCAN

import           Web.DID.Types

import Fission.Web.API.App.Index.Payload.Types

-- | Delegate capabilities to a key pair or DID
delegate ::
  ( MonadIO          m
  , MonadLogger      m 
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

    if appRegistered then
      logDebug $ appName <> " is registered!"
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