-- | Grab files directly from IPFS
module Fission.CLI.Command.Down (command, down) where

import           RIO
import           RIO.ByteString
import           RIO.Directory
import           RIO.Process (HasProcessContext)

import           Data.Has
import           Options.Applicative.Simple (addCommand)
import           System.Console.Haskeline

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
    (const $ runRIO cfg down)
    (pure ())

-- | Sync the current working directory to the server over IPFS
down :: MonadRIO          cfg m
   => HasLogFunc        cfg
   => HasProcessContext cfg
   => Has IPFS.Timeout  cfg
   => Has IPFS.BinPath  cfg
   => Has Client.Runner cfg
   => m ()
down = do
  logDebug "TODO START MESSAGE"
  putStr "ContentId: "
  cid <- getLine

  cid & decodeUtf8Lenient & mkCID & IPFS.getContent >>= \case
    Right content -> logDebug $ displayShow content
    Left  err -> logError $ displayShow err
