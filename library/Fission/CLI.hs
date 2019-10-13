module Fission.CLI (cli) where

import           RIO
import           RIO.Process (HasProcessContext)

import           Data.Has

import           Options.Applicative.Simple

import qualified Fission.Web.Client   as Client
import qualified Fission.IPFS.Types   as IPFS

import qualified Fission.CLI.Command.Login    as Login
import qualified Fission.CLI.Command.Register as Register
import qualified Fission.CLI.Command.Up       as Up
import qualified Fission.CLI.Command.Watch    as Watch

-- | Top-level CLI description
cli :: MonadUnliftIO m
    => HasLogFunc        cfg
    => HasProcessContext cfg
    => Has Client.Runner cfg
    => Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => cfg
    -> IO ((), m ())
cli cfg =
  simpleOptions version description detail (pure ()) do
    Login.command    cfg
    Register.command cfg
    Up.command       cfg
    Watch.command    cfg
  where
    version     = "1.14.0"
    description = "CLI to interact with Fission services"
    detail      = mconcat [ "Fission makes developing, deploying, updating "
                          , "and iterating on web applications quick and easy."
                          ]
