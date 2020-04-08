module Fission.User.Registration.Types ( Registration(..) ) where

import Data.Swagger hiding (email)

import Fission.Prelude
import Fission.User.Email.Types
import Fission.User.Username.Types
import Fission.User.Password.Types

data Registration = Registration
  { username :: !Username
  , email    :: !Email
  , password :: !(Maybe Password)
  }

instance Arbitrary Registration where
  arbitrary = do
    username <- arbitrary
    password <- arbitrary
    email    <- arbitrary

    return Registration {..}

instance ToJSON Registration where
  toJSON Registration { username = Username username', email = Email email' } =
    Object [ ("username", String username')
           , ("email",    String email')
           ]

instance FromJSON Registration where
  parseJSON = withObject "Registration" \obj -> do
    username <- obj .:  "username"
    password <- obj .:? "password"
    email    <- obj .:  "email"

    return Registration {..}

instance ToSchema Registration where
  declareNamedSchema _ = do
    username' <- declareSchemaRef $ Proxy @Text
    email'    <- declareSchemaRef $ Proxy @Text

    mempty
      |> type_      ?~ SwaggerObject
      |> properties .~
           [ ("username", username')
           , ("email", email')
           ]
      |> required .~ ["username", "email"]
      |> description ?~
           "The information that a user needs to provide to login/register."
      |> example ?~ toJSON Registration
           { username = "username"
           , email    = "alice@example.com"
           , password = Nothing
           }
      |> NamedSchema (Just "Registration")
      |> pure
