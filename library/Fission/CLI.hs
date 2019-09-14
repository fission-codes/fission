module Fission.CLI (cli) where

import           RIO

import           Options.Applicative.Simple

import           Fission.Internal.Applicative
import qualified Fission.CLI.Login as Login
import           Fission.CLI.Types

cli :: MonadIO m => Config -> IO ((), m ())
cli cfg =
  simpleOptions version description detail noop do
    Login.command cfg
  where
    version     = "0.0.1"
    description = "Top lines about what the CLI is for"
    detail      = "This CLI does some cool stuff"
