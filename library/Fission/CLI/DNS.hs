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

import           Fission.URL.DomainName.Types

update ::
  ( MonadUnliftIO  m
  , MonadWebClient m
  , MonadLogger    m
  )
  => CID
  -> m (Either ClientError DomainName)
update cid@(CID hash) = do
  logDebug <| "Updating DNS to " <> display hash

  result <- CLI.withLoader 50000 do
    run (DNS.update cid)

  case result of
    Right domain@(DomainName rawDomain) -> do
      CLI.Success.dnsUpdated rawDomain
      return (Right domain)

    Left err -> do
      CLI.Error.put' err
      return (Left err)
