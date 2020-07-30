-- | Initialize a new Fission app in an existing directory

module Fission.CLI.Command.App.Init
  ( cmd
  , cmdTxt
  , appInit
  ) where

import qualified System.Console.ANSI as ANSI
import qualified Crypto.PubKey.Ed25519                  as Ed25519
import           Options.Applicative
import           RIO.FilePath ((</>))

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8

import           Fission.Authorization.ServerDID
import           Fission.URL
import           Fission.Error.AlreadyExists.Types

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client
import           Fission.Web.Client.App                 as App

import qualified Fission.CLI.Display.Error              as CLI.Error
import qualified Fission.CLI.Display.Success            as CLI.Success

import           Fission.CLI.Environment                as Environment
import           Fission.CLI.Environment.Override       as Env.Override
import           Fission.CLI.Prompt.BuildDir

import           Fission.CLI.Command.App.Init.Types     as App.Init
import           Fission.CLI.Command.Types

import           Fission.Internal.Orphanage.ClientError ()

cmd ::
  ( MonadWebClient m
  , MonadIO        m
  , MonadTime      m
  , ServerDID      m
  , MonadWebAuth   m Token
  , MonadWebAuth   m Ed25519.SecretKey
  , MonadLogger    m
  )
  => Command m App.Init.Options ()
cmd = Command
  { command     = cmdTxt
  , description = "Initialize a new Fission app in an existing directory"
  , argParser   = parseOptions
  , handler     = appInit
  }

cmdTxt :: Text
cmdTxt = "app-init"

-- | Sync the current working directory to the server over IPFS
appInit ::
  ( MonadWebClient m
  , MonadIO        m
  , MonadTime      m
  , MonadLogger    m
  , ServerDID      m
  , MonadWebAuth   m Token
  , MonadWebAuth   m Ed25519.SecretKey
  )
  => App.Init.Options
  -> m ()
appInit App.Init.Options {appDir, buildDir, subdomain} = do
  Environment {appURL} <- Environment.get

  case appURL of
    Just url ->
      CLI.Error.put (AlreadyExists @URL) $ "App already exists as " <> textDisplay url
     
    Nothing ->
      sendRequestM (authClient (Proxy @App.Create) `withPayload` subdomain) >>= \case
        Left err ->
          CLI.Error.put err $ textDisplay err

        Right appURL' -> do
          logDebug $ "Created app " <> textDisplay appURL'

          guess <- guessBuildDir appDir

          Env.Override.writeMerge (appDir </> ".fission.yaml") mempty
            { maybeAppURL   = Just appURL'
            , maybeBuildDir = buildDir <|> guess
            }

          CLI.Success.putOk $ "App initialized as " <> textDisplay appURL'

          liftIO do
            UTF8.putText "⏯️  Next run "

            ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
            UTF8.putText "fission up"
            ANSI.setSGR [ANSI.Reset]

            UTF8.putText " or "

            ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
            UTF8.putText "fission watch"
            ANSI.setSGR [ANSI.Reset]

            UTF8.putText " to sync data\n"

            UTF8.putText "💁 It may take DNS time to propagate this initial setup globally. In this case, you can always view your app at "

            ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
            UTF8.putText $ "https://ipfs.runfission.com/ipns/" <> textDisplay appURL' <> "\n"
            ANSI.setSGR [ANSI.Reset]

parseOptions :: Parser App.Init.Options
parseOptions = do
  appDir <- strOption $ mconcat
    [ metavar "PATH"
    , help    "The file path to initialize the app in (app config, etc)"

    , value   "."

    , long    "app-dir"
    , short   'a'
    ]

  optFilePath <- strOption $ mconcat
    [ metavar "PATH"
    , help    "The file path of the assets or directory to sync"

    , value   ""

    , long    "build-dir"
    , short   'b'
    ]

  optSubdomain <- strOption $ mconcat
    [ metavar "SUBDOMAIN"
    , help    "Optional subdomain -- autogeneratd if omitted"

    , value   ""

    , long    "subdomain"
    , short   's'
    ]

  return App.Init.Options
    { appDir
    , buildDir  = mayFilePath optFilePath
    , subdomain = maySubdomain optSubdomain
    }
