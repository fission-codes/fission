-- | Login command
module Fission.CLI.Command.Login (command, login) where

import           RIO
import           RIO.ByteString

import qualified Data.ByteString.Char8 as BS
import           Data.Has
import           Data.List.NonEmpty

import           Options.Applicative.Simple (addCommand)
import           Servant
import           System.Console.Haskeline

import qualified Fission.Config as Config
import           Fission.Internal.Constraint

import           Fission.Web.User.Client  as User.Client
import           Fission.Web.IPFS.Client  as IPFS.Client
import qualified Fission.Web.Client.Types as Client

import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Config.Types

import qualified Fission.CLI.Display.Cursor  as Cursor
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait
import qualified Fission.IPFS.Peer.Types as Peer
import qualified RIO.ByteString.Lazy as Lazy

-- | The command to attach to the CLI tree
command :: MonadIO m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "login"
    "Add your Fission credentials"
    (const $ runRIO cfg login)
    (pure ())

-- | Login (i.e. save credentials to disk). Validates credentials agianst the server.
login :: MonadRIO          cfg m
      => MonadUnliftIO         m
      => HasLogFunc        cfg
      => Has Client.Runner cfg
      => m ()
login = do
  logDebug "Starting login sequence"
  putStr "Username: "
  username <- getLine

  mayPassword <- liftIO $ runInputT defaultSettings $ getPassword (Just '•') "Password: "

  case mayPassword of
    Nothing ->
      logError "Unable to read password"

    Just password -> do
      logDebug "Attempting API verification"
      Client.Runner runner <- Config.get
      let auth = BasicAuthData username $ BS.pack password

      authResult <- Cursor.withHidden
                 . CLI.Wait.waitFor "Verifying your credentials"
                 . runner
                 $ IPFS.Client.peers (IPFS.Client.request auth)

      case authResult of
        Right peers -> do
          let writeTo = UserConfig {username = username
                                    , password = (BS.pack password)
                                    , peers = fromList peers}

          Auth.write writeTo >> CLI.Success.putOk "Logged in"
        Left  err -> CLI.Error.put err "Authorization failed"
