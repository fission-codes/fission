-- | Grab files directly from IPFS
module Fission.CLI.Command.Down (command, down) where

import           Fission.Prelude
import           Network.URI as URI
import           Options.Applicative.Simple (addCommand)
import           Options.Applicative (strArgument, metavar, help)
import qualified Data.Text as Text

import           Network.IPFS
import qualified Network.IPFS.Get as IPFS
import qualified Network.IPFS.Types       as IPFS
import           Network.IPFS.CID.Types

import           Fission.CLI.Config.Base
import           Fission.CLI.Config.Types

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait

-- | The command to attach to the CLI tree
command :: Command m CID ()
command = Command
  { command     = "down"
  , description = "Pull data down to your system"
  , parseArgs   = parseArgs
  , handler     = down
  }
 
parserArgs = strArgument $ mconcat
  [ metavar "ContentID"
  , help    "The CID of the IPFS object you want to download"
  ]

-- | Sync the current working directory to the server over IPFS
down ::
  ( MonadUnliftIO     m
  , MonadLocalIPFS    m
  , MonadLogger       m
  )
  => IPFS.CID
  -> m ()
down (CID identifier) = do
  getResult <- CLI.Wait.waitFor "Retrieving Object..." . IPFS.getFileOrDirectory $ handleIPNS identifier

  case getResult of
    Right _ok ->
      CLI.Success.putOk $ identifier <> " Successfully downloaded!"

    Left err ->
      CLI.Error.put err "Oh no! The download failed unexpectedly"

-- | Return an IPNS address if the identifier is a URI
handleIPNS :: Text -> CID
handleIPNS identifier =
  case URI.parseURI (Text.unpack identifier) of
    Nothing ->
      CID identifier

    Just uri ->
      case uriAuthority uri of
        Just auth -> CID $ "/ipns/" <> Text.pack (uriRegName auth)
        Nothing   -> CID identifier
