module Fission.CLI (cli) where

import           RIO
import           RIO.Process (HasProcessContext)

import           Data.Has

import           Options.Applicative.Simple

import qualified Fission.Web.Client   as Client
import qualified Fission.IPFS.Types   as IPFS

import qualified Fission.CLI.Login as Login
import qualified Fission.CLI.Watch as Watch
import qualified Fission.CLI.Up    as Up

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
    Login.command cfg
    Watch.command cfg
    Up.command    cfg
  where
    version     = "1.10.0"
    description = "CLI to interact with Fission services"
    detail      = mconcat [ "Fission makes developing, deploying, updating "
                          , "and iterating on web applications quick and easy."
                          ]
