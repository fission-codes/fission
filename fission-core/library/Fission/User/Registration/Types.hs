module Fission.User.Registration.Types ( Registration(..) ) where

import qualified Crypto.PubKey.RSA                          as RSA

import           Data.Swagger                               hiding (email)

import           Fission.Prelude

import           Fission.User.Email.Types
import           Fission.User.Password.Types
import           Fission.User.Username.Types

import           Web.Ucan.Internal.Orphanage.RSA2048.Public ()

data Registration = Registration
  { username   :: Username
  , email      :: Email
  , password   :: Maybe Password
  , exchangePK :: Maybe RSA.PublicKey
  }

instance Arbitrary Registration where
  arbitrary = do
    username   <- arbitrary
    password   <- arbitrary
    email      <- arbitrary
    exchangePK <- arbitrary

    return Registration {..}

instance ToJSON Registration where
  toJSON Registration { username, email = Email email' } =
    Object [ ("username", String $ textDisplay username)
           , ("email",    String email')
           ]

instance FromJSON Registration where
  parseJSON = withObject "Registration" \obj -> do
    username   <- obj .:  "username"
    password   <- obj .:? "password"
    email      <- obj .:  "email"
    exchangePK <- obj .:? "exchangePK"

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
      |> description ?~ "The information that a user needs to provide to login/register."
      |> example ?~ toJSON Registration
           { username   = "username"
           , email      = "alice@example.com"
           , password   = Nothing
           , exchangePK = Nothing
           }
      |> NamedSchema (Just "Registration")
      |> pure
