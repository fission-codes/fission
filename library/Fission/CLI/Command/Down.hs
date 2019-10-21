-- | Grab files directly from IPFS
module Fission.CLI.Command.Down (command, down) where

import           RIO
import           RIO.Process (HasProcessContext)

import           Data.Has
import           Options.Applicative.Simple (addCommand)
import           Options.Applicative (strArgument, metavar, help)

import           Fission.Internal.Constraint

import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.IPFS.Types   as IPFS
import           Fission.IPFS.CID.Types
import           Fission.CLI.Config.Types

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait

-- | The command to attach to the CLI tree
command :: MonadRIO          cfg m
        => HasLogFunc        cfg
        => HasProcessContext cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "down"
    "Pull a ipfs or ipns object down to your system"
    (\cid -> runRIO cfg $ down cid)
    (strArgument $ mconcat
      [ metavar "ContentID"
      , help "The CID of the IPFS object you want to download"
      ])

-- | Sync the current working directory to the server over IPFS
down :: MonadUnliftIO         m
     => MonadRIO          cfg m
     => HasLogFunc        cfg
     => HasProcessContext cfg
     => Has IPFS.Timeout  cfg
     => Has IPFS.BinPath  cfg
     => IPFS.CID
     -> m ()
down cid@(CID hash) = do
  getResult <- CLI.Wait.waitFor "Retrieving Object..." $
                IPFS.getFileOrDirectory cid

  case getResult of
    Right _ok ->
      CLI.Success.putOk $ hash <> " Successfully downloaded!"

    Left err ->
      CLI.Error.put err "Oh no! The download failed unexpectedly"
