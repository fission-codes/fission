{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.BasicAuthData () where

import RIO

import Data.Aeson
import Servant

import Fission.Internal.Orphanage.ByteString.Lazy ()


instance ToJSON BasicAuthData where
  toJSON (BasicAuthData username password) =
    Object [ ("username", String $ decodeUtf8Lenient username)
           , ("password", String $ decodeUtf8Lenient password)
           ]

instance FromJSON BasicAuthData where
  parseJSON = withObject "BasicAuthData" \obj ->
    BasicAuthData <$> obj .: "username"
                  <*> obj .: "password"
