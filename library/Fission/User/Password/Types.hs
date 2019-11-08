module Fission.User.Password.Types ( Password(..) ) where

import RIO

import Data.Aeson
import Data.Swagger

newtype Password = Password { password :: Maybe Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToSchema )

instance ToJSON Password where
  toJSON (Password password) = 
    Object [("password", maybe Null String password)]

instance FromJSON Password where
  parseJSON = withObject "Password" \obj -> do
    _password <- obj .:?  "password"

    return $ Password _password
