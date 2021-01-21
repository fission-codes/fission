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
  display (CouldNotSend _) = "An error occured while trying to send an email"


instance ToServerError CouldNotSend where
  toServerError couldNotSend = err500 { errBody = displayLazyBS couldNotSend }
