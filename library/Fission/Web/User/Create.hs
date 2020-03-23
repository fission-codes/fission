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
 
type API = DID_API :<|> PasswordAPI

type DID_API
  =  Summary "Register a new user (must auth with user-controlled DID)"
  :> ReqBody    '[JSON] User.Registration
  :> PutCreated '[JSON] NoContent

type PasswordAPI
  =  Summary "[DEPRECATED] Register a new user (must auth with user-controlled DID)"
  :> ReqBody     '[JSON] User.Registration
  :> PostCreated '[JSON] NoContent

server ::
  ( MonadDNSLink   m
  , MonadLogger    m
  , MonadTime      m
  , MonadDB      t m
  , User.Creator t
  )
  => DID 
  -> ServerT API m
server did = create :<|> createWithPassword
  where
    create (User.Registration {username, email}) = do
      Web.Err.ensure =<< runDBNow (User.create username did email)
      return NoContent

    createWithPassword (User.Registration {username, password, email}) = do
      case password of
        Just pass -> do
          Web.Err.ensure =<< runDBNow (User.createWithPassword username pass email)
          return NoContent

        Nothing ->
          Web.Err.throw err422 { errBody = "Missing password" }
