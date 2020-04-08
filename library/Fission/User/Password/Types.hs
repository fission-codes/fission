module Fission.User.Password.Types (Password (..)) where

import Data.Swagger

import Fission.Prelude

newtype Password = Password { password :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToSchema )

instance Arbitrary Password where
  arbitrary = Password <$> arbitrary

instance ToJSON Password where
  toJSON (Password password) = String password

instance FromJSON Password where
  parseJSON = withText "Password" \txt -> return (Password txt)
