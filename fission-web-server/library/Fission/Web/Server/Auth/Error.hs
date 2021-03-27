module Fission.Web.Server.Auth.Error (Error (..)) where

import           Servant.Server           hiding (BasicAuthResult (..))

import           Fission.Prelude

import           Fission.Web.Server.Error

data Error
  = NoToken
  | CannotParse Text
  | NoSuchUser
  | Unauthorized
  deriving ( Exception
           , Eq
           , Show
           )

instance Display Error where
  textDisplay = \case
    NoToken         -> "No token included on request"
    CannotParse msg -> "Unable to parse token: " <> msg
    NoSuchUser      -> "No such user exists"
    Unauthorized    -> "User not authorized"

instance ToServerError Error where
  toServerError err = err401 { errBody = displayLazyBS err }
