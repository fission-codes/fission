module Fission.Web.Server.Email.Class (MonadEmail (..)) where

import           Servant.Client

import           Fission.Prelude

import           Fission.Challenge.Types

import           Fission.Web.Server.Email.Recipient.Types
import           Fission.Web.Server.Email.SendInBlue.Types as SIB

class Monad m => MonadEmail m where
  sendVerificationEmail :: Recipient -> Challenge -> m (Either ClientError SIB.Response)
