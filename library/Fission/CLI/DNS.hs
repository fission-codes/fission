-- | Update DNS via the CLI
module Fission.CLI.DNS (update) where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Network.IPFS.CID.Types
import           Servant.Client

import           Fission.Prelude
 
import           Fission.Authorization.ServerDID
import           Fission.URL.DomainName.Types as URL

import           Fission.Web.Auth.Token
import           Fission.Web.Client
import           Fission.Web.Routes

import           Fission.CLI.Display.Error   as CLI.Error
import           Fission.CLI.Display.Success as CLI.Success

update ::
  ( MonadUnliftIO  m
  , MonadLogger    m
  , MonadWebClient m
  , MonadTime      m
  , ServerDID      m
  , MonadWebAuth   m Token
  , MonadWebAuth   m Ed25519.SecretKey
  )
  => CID
  -> m (Either ClientError DomainName)
update cid@(CID hash) = do
  logDebug $ "Updating DNS to " <> display hash

  sendRequestM (authClient (Proxy @DNSRoute) `withPayload` cid) >>= \case
    Right domain@(DomainName rawDomain) -> do
      CLI.Success.dnsUpdated rawDomain
      return $ Right domain

    Left err -> do
      CLI.Error.put' err
      return $ Left err
