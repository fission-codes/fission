module Fission.CLI.App (interpret) where

import           Fission.Prelude

import qualified Fission.Internal.UTF8                   as UTF8

import           Fission.CLI.Types
import           Fission.Error

import           Fission.URL.Types

import qualified Fission.CLI.Base.Types                  as Base
import           Fission.CLI.Connected                   as Connected

import           Fission.CLI.App.Environment             as App.Env
import qualified Fission.CLI.Display.Error               as CLI.Error
import           Fission.CLI.Environment                 as CLI.Env hiding (Env)
import           Fission.CLI.Error.Types
import qualified Fission.CLI.Handler                     as Handler
import           Fission.CLI.Handler.Error.Types         (Errs)

import           Fission.CLI.Parser.Command.App          as App
import qualified Fission.CLI.Parser.Command.App.Delegate as App.Delegate
import qualified Fission.CLI.Parser.Command.App.Info     as App.Info
import           Fission.CLI.Parser.Command.App.Init     as App.Init
import           Fission.CLI.Parser.Command.App.Up.Types as App.Up
import qualified Fission.CLI.Parser.Config.IPFS          as IPFS

interpret :: forall errs .
  ( Errs `Contains` errs
  , errs `Contains` errs
  , Display   (OpenUnion errs)
  , Exception (OpenUnion errs)
  )
  => Base.Config
  -> App.Options
  -> FissionCLI errs Base.Config ()
interpret baseCfg cmd = do
  logDebug @Text "App interpreter"

  case cmd of
    Delegate App.Delegate.Options {appName, audienceDid} ->
      Handler.delegate appName audienceDid

    Info App.Info.CommandOnly ->
      Handler.appInfo

    Init App.Init.Options {appDir, buildDir, mayAppName, ipfsCfg = IPFS.Config {..}} -> do
      let
        run' :: FissionCLI errs Connected.Config () -> FissionCLI errs Base.Config ()
        run' = ensureM . Connected.run baseCfg timeoutSeconds

      attempt CLI.Env.alreadySetup >>= \case
        Left errs ->
          case openUnionMatch @NotSetup errs of
            Just err -> do
              CLI.Error.put err "Fission is installed, but you are not logged in. Please run `fission login`"
              raise errs

            Nothing ->
              raise errs

        Right _ ->
          attempt App.Env.read >>= \case
            Right Env {appURL} -> do
              UTF8.putTextLn $ "App already set up at " <> textDisplay appURL
              logDebug . textDisplay $ AlreadyExists @URL
              raise $ AlreadyExists @URL

            Left errs -> do
              case openUnionMatch @(NotFound FilePath) errs of
                Just _ -> do
                  logDebug @Text "Setting up new app"
                  run' $ Handler.appInit appDir buildDir mayAppName

                Nothing -> do
                  logError @Text "Problem setting up new app"
                  raise errs

    Up App.Up.Options {open, watch, updateDNS, updateData, filePath, ipfsCfg = IPFS.Config {..}} -> do
      let
        run' :: FissionCLI errs Connected.Config () -> FissionCLI errs Base.Config ()
        run' = ensureM . Connected.run baseCfg timeoutSeconds

        runIO :: FissionCLI errs Connected.Config a -> IO ()
        runIO = void . Connected.run baseCfg timeoutSeconds

      attempt App.Env.read >>= \case
        Right Env {appURL, ipfsIgnored} ->
          run' . local (addAppIgnore ipfsIgnored) $ -- Local because only need to add for this one scenario
            Handler.publish open watch runIO appURL filePath updateDNS updateData

        Left _ -> do
          CLI.Error.put (NotFound @URL) "You have not set up an app. Please run `fission app register`"
          raise $ NotFound @URL

addAppIgnore :: [Text] -> Connected.Config -> Connected.Config
addAppIgnore appIgnored cfg@Connected.Config {ignoredFiles} =
  cfg {ignoredFiles = ignoredFiles <> appIgnored}
