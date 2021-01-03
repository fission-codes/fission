{-# OPTIONS_GHC -fno-warn-orphans #-}

-- FIXME move to web-client
module Fission.CLI.Internal.Orphanage.BaseUrl () where

import qualified RIO.Text        as Text

-- import           Data.Swagger
import           Servant.Client

import           Fission.Prelude

instance Display BaseUrl where
  textDisplay = Text.pack . show

instance Arbitrary BaseUrl where
  arbitrary = do
    baseUrlScheme <- oneof $ pure <$> [Http, Https]
    baseUrlHost   <- arbitrary
    baseUrlPort   <- arbitrary
    baseUrlPath   <- arbitrary

    return BaseUrl {..}

-- instance ToSchema BaseUrl where
--   declareNamedSchema _ =
--     mempty
--       |> type_   ?~ SwaggerString
--       |> example ?~ toJSON (BaseUrl Client.Https "runfission.com" 443 "")
--       |> NamedSchema (Just "BaseUrl")
--       |> pure
