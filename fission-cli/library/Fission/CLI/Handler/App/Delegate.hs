-- | Delegate capabilities to a key pair or DID
module Fission.CLI.Handler.App.Delegate (delegate) where

import qualified Crypto.PubKey.Ed25519                     as Ed25519
import qualified System.Console.ANSI                       as ANSI

import           Fission.Prelude

import qualified Fission.App.Name                          as App
import           Fission.Authorization.ServerDID
import           Fission.Error.Types
import qualified Fission.Internal.UTF8                     as UTF8

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client

import           Fission.CLI.Display.Text

import qualified Fission.CLI.Display.Error                 as CLI.Error
import qualified Fission.CLI.Display.Success               as CLI.Success

import qualified Fission.CLI.App.Environment               as App.Env
import qualified Fission.CLI.Prompt.BuildDir               as BuildDir

import           Fission.CLI.Environment
import           Fission.CLI.WebNative.Mutation.Auth.Store as UCAN

import           Web.DID.Types

-- | Delegate capabilities to a key pair or DID
delegate ::
  ( MonadLogger m 
  -- , MonadTime        m

  -- , MonadEnvironment m
  -- , UCAN.MonadStore  m
  -- , MonadWebClient   m
  -- , ServerDID        m

  -- , MonadCleanup     m
  -- , m `Raises` ClientError
  -- , m `Raises` NotFound FilePath

  -- , Contains (Errors m) (Errors m)
  -- , Display  (OpenUnion (Errors m))
  -- , Show     (OpenUnion (Errors m))

  -- , MonadWebAuth m Token
  -- , MonadWebAuth m Ed25519.SecretKey
  )
  => Text
  -> Bool 
  -> Maybe DID
  -> m ()
delegate appName generateKey mayAudienceDid = do
    logDebug @Text "delegate"