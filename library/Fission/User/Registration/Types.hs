module Fission.User.Registration.Types ( Registration(..) ) where

import           RIO

import Data.Aeson
import Data.Swagger

import Control.Lens hiding ((.=))

data Registration = Registration
  { _username :: !Text
  , _password :: !Text
  , _email    :: !(Maybe Text)
  }

instance ToJSON Registration where
  toJSON (Registration username' password' maybeEmail) =
    Object [ ("username", String username')
           , ("password", String password')
           , ("email", maybe Null String maybeEmail)
           ]

instance FromJSON Registration where
  parseJSON = withObject "Registration" \obj -> do
    _username <- obj .:  "username"
    _password <- obj .:  "password"
    _email    <- obj .:? "email"

    return $ Registration {..}

instance ToSchema Registration where
  declareNamedSchema _ = do
    username' <- declareSchemaRef $ Proxy @Text
    password' <- declareSchemaRef $ Proxy @Text
    email'    <- declareSchemaRef $ Proxy @Text

    return $ NamedSchema (Just "Registration") $ mempty
      & type_      ?~ SwaggerObject
      & properties .~
          [ ("username", username')
          , ("password", password')
          , ("email", email')
          ]
      & required .~
          [ "username"
          , "password"
          ]
      & description ?~
          "The information that a user needs to provide to login/register."
      & example ?~ toJSON Registration
          { _username = "username"
          , _password = "password123!"
          , _email    = Just "alice@example.com"
          }
