module Fission.Web.Server.Handler.User.Create
  ( create
  , withDID
  , withPassword
  ) where

import           Servant

import           Fission.Prelude

import           Fission.User.DID.Types

import qualified Fission.Web.API.User.Create.Types          as API.User

import qualified Fission.Web.Server.Challenge.Creator.Class as Challenge
import           Fission.Web.Server.Email
import           Fission.Web.Server.Error                   as Web.Err
import           Fission.Web.Server.IPFS.DNSLink            as DNSLink
import qualified Fission.Web.Server.User                    as User

import           Fission.Web.Server.Auth.Types              ()

create ::
  ( MonadDNSLink      m
  , MonadLogger       m
  , MonadTime         m
  , MonadEmail        m
  , User.Creator      m
  , Challenge.Creator m
  )
  => ServerT API.User.Create m
create = withDID :<|> withPassword

withDID ::
  ( MonadDNSLink      m
  , MonadLogger       m
  , MonadTime         m
  , MonadEmail        m
  , User.Creator      m
  , Challenge.Creator m
  )
  => ServerT API.User.CreateWithDID m
withDID User.Registration {username, email} DID {..} = do
  now       <- currentTime
  userId    <- Web.Err.ensureM $ User.create username publicKey email now
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
  => ServerT API.User.CreateWithPassword m
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
