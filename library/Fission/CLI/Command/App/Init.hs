-- | Initialize a new Fission app in an existing directory

module Fission.CLI.Command.App.Init
  ( cmd
  , cmdTxt
  , appInit
  ) where

import qualified Crypto.PubKey.Ed25519                  as Ed25519
import           Options.Applicative
import qualified RIO.Text as Text

import           Fission.Authorization.ServerDID
import           Fission.Prelude

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client
import           Fission.Web.Client.App                 as App

import qualified Fission.CLI.Display.Error              as CLI.Error
import qualified Fission.CLI.Display.Success            as CLI.Success

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
appInit App.Init.Options {appDir, buildDir} = do
  sendRequestM (authClient $ Proxy @App.Create) >>= \case
    Left err ->
      CLI.Error.put err $ textDisplay err

    Right appURL -> do
      logDebug $ "Created app " <> textDisplay appURL
     
      guess <- guessBuildDir appDir

      Env.Override.writeMerge appDir mempty
        { maybeAppURL   = Just appURL
        , maybeBuildDir = buildDir <|> guess
        }

      CLI.Success.putOk $ "App initialized as " <> textDisplay appURL

parseOptions :: Parser App.Init.Options
parseOptions = do
  appDir <- strOption $ mconcat
    [ metavar "PATH"
    , help    "The file path to initialize the app in (app config, etc)"

    , value   "./"

    , long    "app-dir"
    , short   'a'
    ]

  OptionalFilePath buildDir <- strOption $ mconcat
    [ metavar "PATH"
    , help    "The file path of the assets or directory to sync"

    , value   ""

    , long    "build-dir"
    , short   'b'
    ]

  return App.Init.Options {..}
