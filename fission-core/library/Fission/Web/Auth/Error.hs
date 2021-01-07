module Fission.Web.Auth.Error (Error (..)) where

import           Fission.Prelude

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

