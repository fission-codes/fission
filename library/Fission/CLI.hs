module Fission.CLI (cli) where

import           RIO
import           RIO.Process (HasProcessContext)

import           Data.Has

import           Options.Applicative.Simple

import           Fission.Internal.Constraint

import qualified Fission.Web.Client   as Client
import qualified Fission.IPFS.Types   as IPFS

import qualified Fission.CLI.Command.Login    as Login
import qualified Fission.CLI.Command.Logout   as Logout
import qualified Fission.CLI.Command.Register as Register
import qualified Fission.CLI.Command.Up       as Up
import qualified Fission.CLI.Command.Down     as Down
import qualified Fission.CLI.Command.Watch    as Watch
import qualified Fission.CLI.Command.Whoami   as Whoami

-- | Top-level CLI description
cli :: MonadRIO    cfg m
    => MonadUnliftIO m
    => HasLogFunc        cfg
    => HasProcessContext cfg
    => Has Client.Runner cfg
    => Has IPFS.BinPath  cfg
    => Has IPFS.Timeout  cfg
    => m ()
cli = do
  cfg <- ask
  (_, runCLI) <- liftIO $ simpleOptions version description detail (pure ()) do
    Login.command    cfg
    Logout.command    cfg
    Register.command cfg
    Up.command       cfg
    Down.command     cfg
    Watch.command    cfg
    Whoami.command   cfg
  runCLI
  where
    version     = "1.15.1"
    description = "CLI to interact with Fission services"
    detail      = mconcat [ "Fission makes developing, deploying, updating "
                          , "and iterating on web applications quick and easy."
                          ]
