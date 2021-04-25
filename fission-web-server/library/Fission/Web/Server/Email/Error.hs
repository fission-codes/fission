module Fission.Web.Server.Email.Error (CouldNotSend(..)) where

import           Servant.Client
import           Servant.Server

import           Fission.Prelude

import           Fission.Web.Server.Error.Class

newtype CouldNotSend = CouldNotSend ClientError
  deriving ( Show
           , Eq
           )

instance Display CouldNotSend where
  display (CouldNotSend _) = "We couldn't send the verification email, please check you've provided a valid email address."


instance ToServerError CouldNotSend where
  toServerError couldNotSend = err422 { errBody = displayLazyBS couldNotSend }
