-- | Update DNS via the CLI
module Fission.CLI.DNS (update) where

import           Network.IPFS.CID.Types
import           Servant.Client

import           Fission.Prelude

import           Fission.Web.Client
import qualified Fission.Web.Client.DNS  as DNS

import           Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success

import           Fission.URL.DomainName.Types as URL

import           Fission.Prelude

import           Fission.Web.Client      as Client
import qualified Fission.Web.Client.User as User

import qualified Fission.User.Username.Types as User

import           Fission.CLI.Command.Types

import qualified Fission.Key.Store as Key
import qualified Fission.CLI.Config.Connected.Error.Types as Error

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error

import Fission.Authorization.ServerDID
import Fission.Web.Auth.Token
import qualified Crypto.PubKey.Ed25519 as Ed25519


update ::
  ( MonadUnliftIO  m
  , MonadLogger    m
  , MonadWebClient m
  , MonadTime      m
  , MonadLogger    m
  , MonadWebClient m
  , ServerDID m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey

  )
  => CID
  -> m (Either ClientError DomainName)
update cid@(CID hash) = do
  logDebug $ "Updating DNS to " <> display hash

  sendRequestM (authClient DNS.update `withPayload` cid) >>= \case
    Right domain@(DomainName rawDomain) -> do
      CLI.Success.dnsUpdated rawDomain
      return (Right domain)

    Left err -> do
      CLI.Error.put' err
      return (Left err)
