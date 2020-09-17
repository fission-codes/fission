module Fission.CLI.App
  ( interpret
  , Errs
  ) where

import qualified Crypto.PubKey.Ed25519                   as Ed25519
import qualified Data.Yaml                               as YAML
import           Servant.Client.Core

import qualified Network.DNS                             as DNS
import qualified Network.IPFS.Types                      as IPFS

import           Fission.Prelude

import qualified Fission.Internal.UTF8                   as UTF8

import           Fission.CLI.Types
import           Fission.Error
import qualified Fission.Key                             as Key
import           Fission.Models
import           Fission.User.DID.Types

import qualified Fission.IPFS.Error.Types                as IPFS
import           Fission.URL.Types

import qualified Fission.CLI.Base.Types                  as Base
import           Fission.CLI.Connected                   as Connected
import           Fission.CLI.Error.Types

import           Fission.CLI.App.Environment             as App.Env
import qualified Fission.CLI.Display.Error               as CLI.Error
import qualified Fission.CLI.Handler                     as Handler

import           Fission.CLI.Parser.Command.App          as App
import qualified Fission.CLI.Parser.Command.App.Info     as App.Info
import           Fission.CLI.Parser.Command.App.Init     as App.Init
import           Fission.CLI.Parser.Command.App.Up.Types as App.Up
import qualified Fission.CLI.Parser.Config.IPFS          as IPFS

type Errs =
  '[ SomeException
   , IPFS.UnableToConnect
   , ClientError
   , Key.Error
   , NotRegistered
   , NoKeyFile
   , AlreadyExists App
   , DNS.DNSError
   , NotFound DID
   , NotFound URL
   , NotFound FilePath
   , NotFound Ed25519.SecretKey
   , NotFound [IPFS.Peer]
   , YAML.ParseException
   ]

interpret :: forall errs .
  ( Contains Errs errs
  , Contains errs errs
  , Display   (OpenUnion errs)
  , Exception (OpenUnion errs)
  )
  => Base.Config
  -> App.Options
  -> FissionCLI errs Base.Config ()
interpret baseCfg cmd = do
  logDebug @Text "App interpreter"

  case cmd of
    Info (App.Info.Options _) ->
      Handler.appInfo

    Init App.Init.Options {appDir, buildDir, maySubdomain, ipfsCfg = IPFS.Config {..}} -> do
      let
        run' ::
             FissionCLI errs Connected.Config a
          -> FissionCLI errs Base.Config (Either (OpenUnion errs) a)
        run' = Connected.run baseCfg timeoutSeconds

      attempt App.Env.read >>= \case
        Right Env {appURL} -> do
          UTF8.putTextLn $ "App already set up at " <> textDisplay appURL
          logDebug . textDisplay $ AlreadyExists @App
          raise $ AlreadyExists @App

        Left errs -> do
          case openUnionMatch errs of
            Just (_ :: NotFound FilePath) -> do
              logDebug @Text "Setting up new app"
              _ <- run' $ Handler.appInit appDir buildDir maySubdomain
              return ()

            Nothing -> do
              logError @Text "" -- FIXME

    Up App.Up.Options {watch, updateDNS, updateData, filePath, ipfsCfg = IPFS.Config {..}} -> do
      let
        run' :: MonadIO m => FissionCLI errs Connected.Config a -> m ()
        run' = void . Connected.run baseCfg timeoutSeconds

      attempt App.Env.read >>= \case
        Right Env {appURL} ->
          run' $ Handler.publish watch run' appURL filePath updateDNS updateData

        Left _ ->
          CLI.Error.put (NotFound @URL)
            "You have not set up an app. Please run `fission app register`"
