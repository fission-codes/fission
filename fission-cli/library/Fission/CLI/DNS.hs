-- | Update DNS via the CLI
module Fission.CLI.DNS (update) where

import qualified Crypto.PubKey.Ed25519                     as Ed25519
import qualified Data.Yaml                                 as YAML

import           Network.IPFS.CID.Types
import           Servant.Client

import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.Error.Types
import           Fission.URL                               as URL

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client

import qualified Fission.Web.Client.DNS                    as DNS

import           Fission.CLI.Display.Error                 as CLI.Error
import           Fission.CLI.Display.Success               as CLI.Success

import           Fission.CLI.Environment                   (MonadEnvironment)
import           Fission.CLI.WebNative.Mutation.Auth.Store as UCAN

update ::
  ( MonadIO          m
  , MonadTime        m
  , MonadLogger      m
  , MonadWebClient   m
  , UCAN.MonadStore  m
  , MonadEnvironment m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , ServerDID        m

  , MonadCleanup     m
  , m `Raises` ClientError
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  , CheckErrors m
  , Show (OpenUnion (Errors m))
  )
  => CID
  -> m DomainName
update cid@(CID hash) = do
  logDebug $ "Updating DNS to " <> display hash

  proof <- getRootUserProof
  attempt (sendAuthedRequest proof $ DNS.set cid) >>= \case
    Right domainName -> do
      CLI.Success.dnsUpdated $ URL domainName Nothing
      return domainName

    Left err -> do
      CLI.Error.put' err
      raise err

