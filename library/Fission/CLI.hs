module Fission.CLI (cli) where

import           RIO

import SuperRecord

import           Options.Applicative.Simple

import           Fission.Internal.Applicative
import           Fission.Internal.Constraint

import qualified Fission.Web.Client.Types as Client

import qualified Fission.CLI.Login as Login
import qualified Fission.CLI.Up    as Up

-- | Top-level CLI description
cli :: MonadRIO           (Rec cfg) m
    => HasLogFunc         (Rec cfg)
    => Has "fissionAPI" cfg Client.Runner
    => Rec cfg
    -> IO ((), m ())
cli cfg =
  simpleOptions version description detail noop do
    Login.command cfg
    Up.command    cfg
  where
    version     = "1.7.0"
    description = "CLI to interact with Fission services"
    detail      = mconcat [ "Fission makes developing, deploying, updating "
                          , "and iterating on web applications quick and easy."
                          ]
