{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.User.Create
  ( API
  , server
  ) where

import           RIO

import           Database.Selda as Selda

import           Servant
import           Data.Has
import qualified Fission.Config as Config

import           Fission.Web.Server
import qualified Fission.Web.Types       as Web

import qualified Fission.Random as Random

import qualified Fission.User                 as User
import qualified Fission.User.Provision.Types as User

import           Fission.Security.Types (Secret (..))

type API = ReqBody '[JSON] UserRegistration
        :> Post '[JSON] User.Provision


data UserRegistration = UserRegistration
  { _username :: Text
  , _password :: Text
  , _email    :: Text
  }

instance ToJSON UserRegistration where
  toJSON (UserRegistration username password email) =
    Object [ ("username", String $ decodeUtf8Lenient username)
           , ("password", String $ decodeUtf8Lenient password)
           , ("email", String $ decodeUtf8Lenient email)
           ]

instance FromJSON UserRegistration where
  parseJSON = withObject "UserRegistration" \obj ->
    UserRegistration <$> obj .: "username"
                     <*> obj .: "password"
                     <*> obj .: "email"

instance ToSchema UserRegistration where
  declareNamedSchema _ = do
    username' <- declareSchemaRef (Proxy :: Proxy Text)
    password' <- declareSchemaRef (Proxy :: Proxy Text)

    return $ NamedSchema (Just "UserRegistration") $ mempty
      & type_      ?~ SwaggerObject
      & properties .~
          [ ("username", username')
          , ("password", password')
          , ("email", email')
          ]
      & required .~
          [ "username"
          , "password"
          , "email"
          ]
      & description ?~
          "The information that a user needs to provide to login/register."
      & example ?~ toJSON BasicAuthData
          { _username = "username"
          , _password = "password123!"
          , _email = "alice@example.com"
          }



server :: HasLogFunc      cfg
       => Has Web.Host    cfg
       => MonadSelda (RIO cfg)
       => RIOServer       cfg API
server (UserRegistration username password email) = do
  Web.Host url <- Config.get
  userID       <- User.createWithEmail username password email
  logInfo $ "Provisioned user: " <> displayShow userID

  return User.Provision
    { _url      = url
    , _username = username
    , _password = Secret password
    }
