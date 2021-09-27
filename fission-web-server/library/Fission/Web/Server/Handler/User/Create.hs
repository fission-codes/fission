module Fission.Web.Server.Handler.User.Create
  ( createV_
  , withDID
  , withPassword
  ) where

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import           Fission.User.DID.Types                     as DID

import qualified Fission.Web.API.User.Create.Types          as User.Create

import qualified Fission.Web.Server.Challenge.Creator.Class as Challenge
import           Fission.Web.Server.Email
import           Fission.Web.Server.Error                   as Web.Err
import           Fission.Web.Server.IPFS.DNSLink            as DNSLink
import qualified Fission.Web.Server.User                    as User

import           Fission.Web.Server.Auth.Types              ()

createV_ ::
  ( MonadDNSLink      m
  , MonadLogger       m
  , MonadTime         m
  , MonadEmail        m
  , User.Creator      m
  , Challenge.Creator m
  )
  => User.Create.RoutesV_ (AsServerT m)
createV_ =
  User.Create.RoutesV_
    { withDID      = withDID
    , withPassword = withPassword
    }

withDID ::
  ( MonadDNSLink      m
  , MonadLogger       m
  , MonadTime         m
  , MonadEmail        m
  , User.Creator      m
  , Challenge.Creator m
  )
  => ServerT User.Create.WithDID m
withDID User.Registration {username, email} (DID.Key publicKey) = do
  now       <- currentTime
  userId    <- Web.Err.ensureM $ User.create username publicKey email now
  challenge <- Web.Err.ensureM $ Challenge.create userId
  Web.Err.ensureM $ sendVerificationEmail (Recipient email username) challenge
  return NoContent

withPassword ::
  ( MonadDNSLink      m
  , MonadLogger       m
  , MonadTime         m
  , MonadEmail        m
  , User.Creator      m
  , Challenge.Creator m
  )
  => ServerT User.Create.WithPassword m
withPassword User.Registration {password = Nothing} =
  Web.Err.throw err422 { errBody = "Missing password" }

withPassword User.Registration {username, password = Just pass, email} = do
  now <- currentTime
  userId <- Web.Err.ensureM $ User.createWithPassword username pass email now
  challenge <- Web.Err.ensureM $ Challenge.create userId
  Web.Err.ensureM $ sendVerificationEmail (Recipient email username) challenge
  return ()
