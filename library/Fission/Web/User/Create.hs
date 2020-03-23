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

type PutAPI
  =  Summary "Register a new user (must auth with user-controlled DID)"
  :> ReqBody    '[JSON] User.Registration
  :> PutCreated '[JSON] NoContent

type PostAPI
  =  Summary "[DEPRECATED] Register a new user (must auth with user-controlled DID)"
  :> ReqBody    '[JSON] User.Registration
  :> PostCreated '[JSON] NoContent

type API = PutAPI :<|> PostAPI

server ::
  ( MonadDNSLink   m
  , MonadLogger    m
  , MonadTime      m
  , MonadDB      t m
  , User.Creator t
  )
  => DID 
  -> ServerT API m
server did = serverPut did :<|> serverPost did

serverPut ::
  ( MonadDNSLink   m
  , MonadLogger    m
  , MonadTime      m
  , MonadDB      t m
  , User.Creator t
  )
  => DID
  -> ServerT PutAPI m
serverPut did (User.Registration username email) = do
  Web.Err.ensure =<< runDBNow (User.create username did email)
  return NoContent

serverPost ::
  ( MonadDNSLink   m
  , MonadLogger    m
  , MonadTime      m
  , MonadDB      t m
  , User.Creator t
  )
  => DID
  -> ServerT PostAPI m
serverPost did (User.Registration username email) = do
  Web.Err.ensure =<< runDBNow (User.create username did email)
  return NoContent
