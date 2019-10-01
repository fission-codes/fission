-- | File sync, IPFS-style
module Fission.CLI.Command.Up (command, up) where

import           RIO
import           RIO.Directory
import           RIO.Process (HasProcessContext)

import           Data.Has
import           Options.Applicative.Simple (addCommand)

import           Fission.Internal.Constraint

import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Web.Client   as Client

import qualified Fission.CLI.Auth    as Auth
import qualified Fission.CLI.Pin     as CLI.Pin
import           Fission.CLI.Config.Types

-- | The command to attach to the CLI tree
command :: MonadIO m
        => HasLogFunc        cfg
        => HasProcessContext cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => Has Client.Runner cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "up"
    "Keep your current working directory up"
    (const $ runRIO cfg up)
    (pure ())

-- | Sync the current working directory to the server over IPFS
up :: MonadRIO          cfg m
   => HasLogFunc        cfg
   => HasProcessContext cfg
   => Has IPFS.Timeout  cfg
   => Has IPFS.BinPath  cfg
   => Has Client.Runner cfg
   => m ()
up = do
  logDebug "Starting single IPFS add locally"
  dir <- getCurrentDirectory
  IPFS.addDir dir >>= \case
    Right cid -> Auth.withAuth (void . CLI.Pin.run cid)
    Left  err -> logError $ displayShow err
