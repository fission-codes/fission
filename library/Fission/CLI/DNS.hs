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

update ::
  ( MonadUnliftIO  m
  , MonadLogger    m
  , MonadWebRequest                    req m
  , MonadAuthedEndpoint URL.DomainName req

  )
  => CID
  -> m (Either ClientError DomainName)
update cid@(CID hash) = do
  logDebug $ "Updating DNS to " <> display hash

  response <- CLI.withLoader 50000 do
    sendRequest . withAuth ucanJWT $ toEndpoint DNS.update cid

  case response of
    Right domain@(DomainName rawDomain) -> do
      CLI.Success.dnsUpdated rawDomain
      return (Right domain)

    Left err -> do
      CLI.Error.put' err
      return (Left err)
