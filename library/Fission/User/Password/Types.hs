module Fission.User.Password.Types ( Password(..) ) where

import RIO

import Data.Aeson
import Data.Swagger

newtype Password = Password { password :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToSchema )

instance ToJSON Password where
  toJSON (Password password) = toJSON $ String password

instance FromJSON Password where
  parseJSON = withText "Password" \txt -> return $ Password txt
