module Fission.Email.Class (MonadEmail (..)) where

import           Fission.Prelude
import           Servant.Client

import           Fission.Email.Recipient.Types
import           Fission.Email.SendInBlue.Types as SIB
import           Fission.Challenge.Types

class Monad m => MonadEmail m where
  sendVerificationEmail :: Recipient -> Challenge -> m (Either ClientError SIB.Response)
