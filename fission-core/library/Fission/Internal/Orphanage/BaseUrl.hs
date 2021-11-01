{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.BaseUrl () where

import qualified RIO.Text            as Text

import           Data.Swagger
import           Servant.Client.Core as Client

import           Fission.Prelude

instance Display BaseUrl where
  textDisplay = Text.pack . showBaseUrl

instance Arbitrary BaseUrl where
  arbitrary = do
    baseUrlScheme <- oneof $ pure <$> [Client.Http, Client.Https]
    baseUrlHost   <- arbitrary
    baseUrlPort   <- arbitrary
    baseUrlPath   <- arbitrary

    return BaseUrl {..}

instance ToSchema BaseUrl where
  declareNamedSchema _ =
    mempty
      |> type_   ?~ SwaggerString
      |> example ?~ toJSON (BaseUrl Client.Https "runfission.com" 443 "")
      |> NamedSchema (Just "BaseUrl")
      |> pure
