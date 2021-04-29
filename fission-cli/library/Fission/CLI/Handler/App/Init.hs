-- | Initialize a new Fission app in an existing directory
module Fission.CLI.Handler.App.Init (appInit) where

import qualified Crypto.PubKey.Ed25519                     as Ed25519
import qualified Data.Yaml                                 as YAML
import qualified System.Console.ANSI                       as ANSI

import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.Error.Types
import qualified Fission.Internal.UTF8                     as UTF8
import qualified Fission.Web.Client.App                    as App

import qualified Fission.App.Name                          as App

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client

import           Fission.CLI.Display.Text

import qualified Fission.CLI.Display.Error                 as CLI.Error
import qualified Fission.CLI.Display.Success               as CLI.Success

import qualified Fission.CLI.App.Environment               as App.Env
import qualified Fission.CLI.Prompt.BuildDir               as BuildDir

import           Fission.CLI.Environment
import           Fission.CLI.WebNative.Mutation.Auth.Store as UCAN

-- | Sync the current working directory to the server over IPFS
appInit ::
  ( MonadIO          m
  , MonadTime        m
  , MonadLogger      m
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
  => FilePath
  -> Maybe FilePath
  -> Maybe App.Name
  -> m ()
appInit appDir mayBuildDir' mayAppName = do
  logDebug @Text "appInit"

  proof <- getRootUserProof
  attempt (sendAuthedRequest proof $ App.create mayAppName) >>= \case
    Left err -> do
      logDebug $ textDisplay err
      CLI.Error.put err $ textDisplay err
      raise err

    Right appURL -> do
      logDebug $ "Created app " <> textDisplay appURL

      case mayBuildDir' of
        Nothing ->  do
          guess <- BuildDir.prompt appDir
          App.Env.create appURL $ fromMaybe guess mayBuildDir'

        Just dir -> do
          logDebug $ "BuildDir passed from flag: " <> dir
          App.Env.create appURL dir

      CLI.Success.putOk $ "App initialized as " <> textDisplay appURL

      UTF8.putText "⏯️  Next run "

      colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] do
        UTF8.putText "fission app publish [--open|--watch]"

      UTF8.putText " to sync data\n"

      UTF8.putText "💁 It may take DNS time to propagate this initial setup globally. In this case, you can always view your app at "

      colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] do
        UTF8.putText $ "https://ipfs.runfission.com/ipns/" <> textDisplay appURL <> "\n"

      return ()
