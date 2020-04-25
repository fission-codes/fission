module Fission.Web.Auth.Error (Error (..)) where

import qualified Servant.Server as Server

import           Fission.Prelude
import           Fission.Web.Error as Error

data Error
  = NoToken
  | BadToken
  | NoSuchUser
  | Unauthorized
  deriving ( Exception
           , Eq
           , Show
           )

instance Display Error where
  display = \case
    NoToken      -> "No token included on request"
    BadToken     -> "Token is improperly formatted"
    NoSuchUser   -> "No such user exists"
    Unauthorized -> "User not authorized"

instance ToServerError Error where
  toServerError err = Error.withMessage err Server.err401
