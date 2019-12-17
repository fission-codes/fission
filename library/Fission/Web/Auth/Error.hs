module Fission.Web.Auth.Error (Error (..)) where

import Fission.Prelude

data Error
  = NoToken
  | BadToken
  | NoSuchUser
  | Unauthorized
  deriving ( Exception
           , Eq
           , Generic
           , ToJSON
           )

instance Show Error where
  show = \case
    NoToken      -> "No token included on request"
    BadToken     -> "Token is improperly formatted"
    NoSuchUser   -> "No such user exists"
    Unauthorized -> "User not authorized"
