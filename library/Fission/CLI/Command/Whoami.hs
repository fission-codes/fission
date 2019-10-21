-- | Whoami command
module Fission.CLI.Command.Whoami (command, whoami) where

import           RIO
import           RIO.ByteString

import           Options.Applicative.Simple (addCommand)
import           Servant

import qualified System.Console.ANSI as ANSI

import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Config.Types

-- | The command to attach to the CLI tree
command :: MonadIO m
        => HasLogFunc cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "whoami"
    "Check the current user"
    (const $ runRIO cfg whoami)
    (pure ())

whoami :: MonadRIO   cfg m
       => HasLogFunc cfg
       => m ()
whoami = do
  Auth.get >>= \case
    Left err -> do
      logError $ displayShow err
      Auth.couldNotRead

    Right auth -> do
      UTF8.putText "💻 Currently logged in as: "

      liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
      putStr $ basicAuthUsername auth <> "\n"

      liftIO $ ANSI.setSGR [ANSI.Reset]
