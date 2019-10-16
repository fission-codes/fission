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
import           System.Environment

import           Fission.Internal.Constraint

import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Web.Client   as Client

import qualified Fission.CLI.Auth    as Auth
import qualified Fission.CLI.Pin     as CLI.Pin
import           Fission.CLI.Config.Types
import qualified Fission.CLI.Display.Cursor  as Cursor
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait

-- | The command to attach to the CLI tree
command :: MonadUnliftIO m
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
    (strArgument $ metavar "ContentID" <> help "The CID of the IPFS object you want to download")

-- | Sync the current working directory to the server over IPFS
down :: MonadRIO        cfg m
   => MonadUnliftIO         m
   => HasLogFunc        cfg
   => HasProcessContext cfg
   => Has IPFS.Timeout  cfg
   => Has IPFS.BinPath  cfg
   => Has Client.Runner cfg
   => IPFS.CID
   -> m ()
down cid@(IPFS.CID hash) = do
  getResult <- CLI.Wait.waitFor "Retrieving Object..."
              $ IPFS.getContent cid

  case getResult of
    Right _ok ->
      CLI.Success.putOk $ hash <> " Successfully downloaded!"
    Left  err ->
      CLI.Error.put err "Oh no! The download failed unexpectedly"
