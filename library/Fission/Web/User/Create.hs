module Fission.Web.User.Create
  ( API
  , server
  ) where

import           Servant

import           Fission.Prelude

import           Fission.Web.Error    as Web.Err
import           Fission.IPFS.DNSLink as DNSLink

import qualified Fission.User as User
import           Fission.User.DID.Types

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
server did (User.Registration username email) = do
  Web.Err.ensure =<< runDBNow (User.create username did email)
  return NoContent
