module Fission.Web.User.Password.Reset.Types (Reset (..)) where

import RIO

import Control.Lens

import Data.Aeson
import Data.Swagger

import qualified Fission.User.Password.Types as User

newtype Reset = Reset { maybePassword :: Maybe User.Password }
  deriving Show

instance FromJSON Reset where
  parseJSON = withObject "User.Password.Reset" \obj -> do
    mayPassword <- obj .:? "password"
    return $ Reset mayPassword

instance ToSchema Reset where
  declareNamedSchema _ = do
    password <- declareSchemaRef $ Proxy @Text
    return $ NamedSchema (Just "Password Reset Request") $ mempty
      & type_       ?~ SwaggerObject
      & description ?~ "Password Reset Request"
      & properties  .~ [("password", password)]
      & example     ?~ "{ password: \"12345\" }"
