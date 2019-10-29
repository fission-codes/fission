{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.BasicAuthData () where

import RIO

import Data.Aeson
import Servant
import Data.Swagger
import Control.Lens hiding ((.=))

import Fission.Internal.Orphanage.ByteString.Lazy ()

instance ToJSON BasicAuthData where
  toJSON (BasicAuthData username password) =
    Object [ ("username", String $ decodeUtf8Lenient username)
           , ("password", String $ decodeUtf8Lenient password)
           ]

instance FromJSON BasicAuthData where
  parseJSON = withObject "BasicAuthData" \obj -> do
    basicAuthUsername <- obj .: "username"
    basicAuthPassword <- obj .: "password"

    return $ BasicAuthData {..}

instance ToSchema BasicAuthData where
  declareNamedSchema _ = do
    username' <- declareSchemaRef $ Proxy @Text
    password' <- declareSchemaRef $ Proxy @Text

    return $ NamedSchema (Just "BasicAuthData") $ mempty
      & type_      ?~ SwaggerObject
      & properties .~
          [ ("username", username')
          , ("password", password')
          ]
      & required .~
          [ "username"
          , "password"
          ]
      & description ?~
          "The information that a user needs to provide to login/register."
      & example ?~ toJSON BasicAuthData
          { basicAuthUsername = "username"
          , basicAuthPassword = "password123!"
          }
