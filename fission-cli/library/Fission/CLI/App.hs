module Fission.CLI.App
  ( interpret
  , Errs
  ) where

import qualified Crypto.PubKey.Ed25519                   as Ed25519
import qualified Network.IPFS.Types                      as IPFS
import           Servant.Client.Core

import           Fission.Prelude

import qualified Fission.Internal.UTF8                   as UTF8

import           Fission.CLI.Types
import           Fission.Error
import qualified Fission.Key                             as Key
import           Fission.Models

import qualified Fission.CLI.Base.Types                  as Base
import           Fission.CLI.Connected                   as Connected
import           Fission.CLI.Error.Types
import           Fission.URL.Types

import qualified Fission.CLI.Display.Error               as CLI.Error
import           Fission.CLI.Environment                 as Environment
import qualified Fission.CLI.Handler                     as Handler
import qualified Fission.IPFS.Error.Types                as IPFS
import qualified Fission.IPFS.Local                      as IPFS.Local

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
   , NotFound URL
   , NotFound Ed25519.SecretKey
   , NotFound [IPFS.Peer]
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
  Environment {appURL} <- Environment.get

  case cmd of
    Info (App.Info.Options _) ->
      Handler.appInfo

    Init App.Init.Options {appDir, buildDir, maySubdomain, ipfsCfg = IPFS.Config {..}} -> do
      binPath' <- IPFS.Local.toBinPath binPath

      let
        run' :: FissionCLI errs Connected.Config a -> FissionCLI errs Base.Config ()
        run' = void . Connected.run baseCfg binPath' timeoutSeconds

      case appURL of
        Nothing ->
          run' $ Handler.appInit appDir buildDir maySubdomain

        Just url -> do
          UTF8.putTextLn $ "App already set up at " <> textDisplay url
          logDebug . textDisplay $ AlreadyExists @App
          raise $ AlreadyExists @App

    Up App.Up.Options {watch, updateDNS, updateData, filePath, ipfsCfg = IPFS.Config {..}} -> do
      binPath' <- IPFS.Local.toBinPath binPath

      let
        run' :: MonadIO m => FissionCLI errs Connected.Config a -> m ()
        run' = void . Connected.run baseCfg binPath' timeoutSeconds

      case appURL of
        Nothing ->
          CLI.Error.put (NotFound @URL)
            "You have not set up an app. Please run `fission app register`"

        Just url ->
          run' $ Handler.up watch run' url filePath updateDNS updateData
