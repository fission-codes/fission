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

import qualified Fission.Challenge.Creator.Class as Challenge

import           Fission.Email

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
  ( MonadDNSLink      m
  , MonadLogger       m
  , MonadTime         m
  , MonadEmail        m
  , User.Creator      m
  , Challenge.Creator m
  )
  => DID
  -> ServerT API m
withDID DID {..} User.Registration {username, email} = do
  now <- currentTime
  userId <- Web.Err.ensureM $ User.create username publicKey email now
  challenge <- Web.Err.ensureM $ Challenge.create userId

  sendVerificationEmail (Recipient email username) challenge >>= \case
    Left _ ->
      Web.Err.throw err500 { errBody = "Could not send verification email" }

    Right _ -> 
      return NoContent

withPassword ::
  ( MonadDNSLink      m
  , MonadLogger       m
  , MonadTime         m
  , MonadEmail        m
  , User.Creator      m
  , Challenge.Creator m
  )
  => ServerT PasswordAPI m
withPassword User.Registration {password = Nothing} =
  Web.Err.throw err422 { errBody = "Missing password" }

withPassword User.Registration {username, password = Just pass, email} = do
  now <- currentTime
  userId <- Web.Err.ensureM $ User.createWithPassword username pass email now
  challenge <- Web.Err.ensureM $ Challenge.create userId

  sendVerificationEmail (Recipient email username) challenge >>= \case
    Left _ -> 
      Web.Err.throw err500 { errBody = "Could not send verification email" }
    Right _ -> 
      return ()
