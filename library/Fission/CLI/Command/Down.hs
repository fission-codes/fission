-- | Grab files directly from IPFS
module Fission.CLI.Command.Down (command, down) where

import           RIO
import           RIO.ByteString
import           RIO.Directory
import           RIO.Process (HasProcessContext)

import           Data.Has
import           Options.Applicative.Simple (addCommand)
import           Options.Applicative (strArgument, metavar, help)
import           System.Console.Haskeline
import System.Environment

import           Fission.Internal.Constraint

import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Web.Client   as Client
import           Fission.IPFS.CID.Types (mkCID)

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
    "down"
    "pull a ipfs or ipns object down to your system"
    (\cid -> runRIO cfg $ down cid)
    (strArgument (metavar "ContentID" <> help "The CID of the IPFS object you want to download")) -- I would like to get this value

-- | Sync the current working directory to the server over IPFS
down :: MonadRIO          cfg m
   => HasLogFunc        cfg
   => HasProcessContext cfg
   => Has IPFS.Timeout  cfg
   => Has IPFS.BinPath  cfg
   => Has Client.Runner cfg
   => IPFS.CID
   -> m ()
down cid = do
  logDebug "TODO START MESSAGE"
  IPFS.getContent cid >>= \case
    Right content -> logDebug $ displayShow content
    Left  err -> logError $ displayShow err
