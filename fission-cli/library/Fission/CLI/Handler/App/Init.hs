-- | Initialize a new Fission app in an existing directory
module Fission.CLI.Handler.App.Init (appInit) where

import qualified Crypto.PubKey.Ed25519           as Ed25519
import qualified System.Console.ANSI             as ANSI

import           Fission.Prelude

import qualified Fission.Internal.UTF8           as UTF8
import qualified Fission.Web.Client.App          as App

import           Fission.Authorization.ServerDID
import           Fission.URL                     as URL

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client

import           Fission.CLI.Display.Text

import qualified Fission.CLI.Display.Error       as CLI.Error
import qualified Fission.CLI.Display.Success     as CLI.Success

import qualified Fission.CLI.App.Environment     as App.Env
import qualified Fission.CLI.Prompt.BuildDir     as BuildDir

type Errs = '[
  ClientError
]

-- | Sync the current working directory to the server over IPFS
appInit ::
  ( MonadIO        m
  , MonadTime      m
  , MonadLogger    m

  , MonadWebClient m
  , ServerDID      m

  , MonadCleanup   m
  , m `Raises` ClientError

  , Contains (Errors m) (Errors m)
  , Display  (OpenUnion (Errors m))
  , Show     (OpenUnion (Errors m))

  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => FilePath
  -> Maybe FilePath
  -> Maybe URL.Subdomain
  -> m (Either (OpenUnion Errs) ())
appInit appDir mayBuildDir' maySubdomain = do
  logDebug @Text "appInit"

  attempt (sendAuthedRequest (App.create maySubdomain)) >>= \case -- maySubdomain)) >>= \case
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

      colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow] do
        UTF8.putText "fission app publish"

      UTF8.putText " or "

      colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow] do
        UTF8.putText "fission app publish --watch"

      UTF8.putText " to sync data\n"

      UTF8.putText "💁 It may take DNS time to propagate this initial setup globally. In this case, you can always view your app at "

      colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] do
        UTF8.putText $ "https://ipfs.runfission.com/ipns/" <> textDisplay appURL <> "\n"

      return ()
