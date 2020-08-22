-- | Initialize a new Fission app in an existing directory
module Fission.CLI.Handler.App.Init (appInit) where

import qualified Crypto.PubKey.Ed25519                  as Ed25519
import           Options.Applicative
import           RIO.FilePath                           ((</>))
import qualified System.Console.ANSI                    as ANSI

import qualified Servant.Client                         as Servant

import qualified Fission.Internal.UTF8                  as UTF8
import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.URL                            as URL

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client
import           Fission.Web.Client.App                 as App

import qualified Fission.CLI.Display.Error              as CLI.Error
import qualified Fission.CLI.Display.Success            as CLI.Success

import           Fission.CLI.Environment.Override       as Env.Override
import           Fission.CLI.Prompt.BuildDir

import           Fission.Internal.Orphanage.ClientError ()

-- | Sync the current working directory to the server over IPFS
appInit ::
  ( MonadIO        m
  , MonadTime      m
  , MonadLogger    m

  , MonadWebClient m
  , ServerDID      m

  , MonadCleanup   m
  , m `Raises` Servant.ClientError
  , Contains (Errors m) (Errors m)
  , Display (OpenUnion (Errors m))
  , Show (OpenUnion (Errors m))

  , MonadWebAuth   m Token
  , MonadWebAuth   m Ed25519.SecretKey
  )
  => FilePath
  -> Maybe FilePath
  -> Maybe URL.Subdomain
  -> m ()
appInit appDir mayBuildDir' maySubdomain = do
  attempt (sendRequestM $ authClient (Proxy @App.Create) `withPayload` maySubdomain) >>= \case
    Left err -> do
      CLI.Error.put err $ textDisplay err
      raise err

    Right appURL' -> do
      logDebug $ "Created app " <> textDisplay appURL'

      guess <- guessBuildDir appDir

      Env.Override.writeMerge (appDir </> ".fission.yaml") mempty
        { maybeAppURL   = Just appURL'
        , maybeBuildDir = mayBuildDir' <|> guess
        }

      CLI.Success.putOk $ "App initialized as " <> textDisplay appURL'

      liftIO do
        UTF8.putText "⏯️  Next run "

        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
        UTF8.putText "fission app publish"
        ANSI.setSGR [ANSI.Reset]

        UTF8.putText " or "

        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
        UTF8.putText "fission app publish --watch"
        ANSI.setSGR [ANSI.Reset]

        UTF8.putText " to sync data\n"

        UTF8.putText "💁 It may take DNS time to propagate this initial setup globally. In this case, you can always view your app at "

        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
        UTF8.putText $ "https://ipfs.runfission.com/ipns/" <> textDisplay appURL' <> "\n"
        ANSI.setSGR [ANSI.Reset]

        return ()
