module Fission.Web.User.Create
  ( API
  , PasswordAPI
  , withDID
  , withPassword
  ) where

import           Servant

import           Fission.Prelude

import           Fission.IPFS.DNSLink   as DNSLink
import           Fission.Web.Error      as Web.Err

import qualified Fission.User           as User
import           Fission.User.DID.Types

type API
  =  Summary "Create user with DID and UCAN proof"
  :> Description "Register a new user (must auth with user-controlled DID)"
  :> ReqBody    '[JSON] User.Registration
  :> PutCreated '[JSON] NoContent

type PasswordAPI
  =  Summary "Create user with password"
  :> Description "DEPRECATED ⛔ Register a new user (must auth with user-controlled DID)"
  :> ReqBody     '[JSON] User.Registration
  :> PostCreated '[JSON] ()

withDID ::
  ( MonadDNSLink m
  , MonadLogger  m
  , MonadTime    m
  , User.Creator m
  )
  => DID
  -> ServerT API m
withDID DID {..} User.Registration {username, email} = do
  now <- currentTime
  Web.Err.ensureM $ User.create username publicKey email now
  return NoContent

withPassword ::
  ( MonadDNSLink m
  , MonadLogger  m
  , MonadTime    m
  , User.Creator m
  )
  => ServerT PasswordAPI m
withPassword User.Registration {password = Nothing} =
  Web.Err.throw err422 { errBody = "Missing password" }

withPassword User.Registration {username, password = Just pass, email} = do
  now <- currentTime
  Web.Err.ensureM $ User.createWithPassword username pass email now
  return ()
