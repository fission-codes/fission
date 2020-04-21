-- | Pin files via the CLI
module Fission.CLI.IPFS.Pin (add) where

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Network.IPFS.CID.Types
import           Servant.Client

import           Fission.Prelude
import           Fission.Authorization.ServerDID

import           Fission.Web.Auth.Token
import           Fission.Web.Client      as Client
import qualified Fission.Web.Client.IPFS as Fission

import           Fission.CLI.Display.Error   as CLI.Error
import           Fission.CLI.Display.Success as CLI.Success

add ::
  ( MonadUnliftIO  m
  , MonadTime      m
  , MonadLogger    m
  , MonadWebClient m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  , ServerDID m
  )
  => CID
  -> m (Either ClientError CID)
add cid@(CID hash) = do
  logDebug $ "Remote pinning " <> display hash
  sendRequestM (authClient Fission.pin `withPayload` cid) >>= \case
    Right _ -> do
      CLI.Success.live hash
      return $ Right cid

    Left err -> do
      CLI.Error.put' err
      return $ Left err
