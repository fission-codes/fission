module Fission.Web.User.Create
  ( API
  , server
  ) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude

import           Fission.Web.Error    as Web.Err
import           Fission.IPFS.DNSLink as DNSLink

import qualified Fission.User as User
import           Fission.User.DID.Types
import           Fission.User.Username.Types

type API = ReqBody '[JSON] User.Registration
        :> PutCreated '[JSON] NoContent

server ::
  ( MonadDNSLink   m
  , MonadLogger    m
  , MonadTime      m
  , MonadDB      t m
  , User.Creator t
  )
  => DID
  -> ServerT API m
server did (User.Registration username@(Username rawUN) email) = do
  Web.Err.ensure =<< runDBNow (User.create username did email)
  Web.Err.ensureM =<< DNSLink.setWithSubdomain rawUN splashCID
  return NoContent

splashCID :: CID
splashCID = CID "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"
