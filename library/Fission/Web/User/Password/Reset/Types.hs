module Fission.Web.User.Password.Reset.Types (Reset (..)) where

import           Data.Swagger

import           Fission.Prelude
import qualified Fission.User.Password.Types as User

newtype Reset = Reset { maybePassword :: Maybe User.Password }
  deriving Show

instance Arbitrary Reset where
  arbitrary = Reset <$> arbitrary

instance ToJSON Reset where
  toJSON (Reset password) =
    Object [("password", maybe Null (String . User.password) password)]

instance FromJSON Reset where
  parseJSON = withObject "User.Password.Reset" \obj -> do
    mayPassword <- obj .:? "password"
    return $ Reset mayPassword

instance ToSchema Reset where
  declareNamedSchema _ = do
    password <- declareSchemaRef $ Proxy @Text
    mempty
      |> type_       ?~ SwaggerObject
      |> description ?~ "Password Reset Request"
      |> properties  .~ [("password", password)]
      |> NamedSchema (Just "PasswordResetRequest")
      |> pure
