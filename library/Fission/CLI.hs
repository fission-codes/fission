module Fission.CLI (cli) where

import           Options.Applicative.Simple
import qualified RIO.Text                     as Text

import qualified Fission.Internal.CLI.Meta    as Meta
import           Fission.Prelude

import           Fission.CLI.Config.Base
import           Fission.CLI.Config.Connected

import           Fission.CLI.Command          as Command
import           Fission.CLI.Display.Error

import qualified Fission.CLI.Command.App.Init as App.Init
import qualified Fission.CLI.Command.Down     as Down
import qualified Fission.CLI.Command.Setup    as Setup
import qualified Fission.CLI.Command.Up       as Up
import qualified Fission.CLI.Command.Watch    as Watch
import qualified Fission.CLI.Command.Whoami   as Whoami

cli :: MonadIO m => BaseConfig -> m ()
cli baseCfg = liftIO do
  (_, runCLI) <- simpleOptions version summary detail noop do
    runBase_ Setup.cmd

    runConnected_ Whoami.cmd
    runConnected_ Up.cmd
    runConnected_ Down.cmd
    runConnected_ App.Init.cmd
    runConnected_ (Watch.cmd (void . runConnected baseCfg))

  runCLI

  where
    runBase_ :: Command FissionBase input () -> Command.Leaf
    runBase_ = runWith (runBase baseCfg)

    runConnected_ :: Command FissionConnected input () -> Command.Leaf
    runConnected_ = runWith \actn ->
      runConnected baseCfg actn >>= \case
        Right _  -> return ()
        Left err -> void . runConnected baseCfg $ put' err

summary :: String
summary = "CLI to interact with Fission services"

detail :: String
detail = "Fission makes developing, deploying, updating, and iterating on web apps quick and easy."

version :: String
version = Text.unpack $ maybe "unknown" identity (Meta.version =<< Meta.package)
