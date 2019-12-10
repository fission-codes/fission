{-# LANGUAGE NoDeriveAnyClass #-}
module Fission.User.Types (User(..)) where

import Data.Swagger
import Fission.Prelude

-- Database

import qualified Fission.Storage.Database as Database

-- Model Dependencies

import qualified Fission.Platform.Heroku.AddOn.Types as Heroku

-- User instances

import qualified Fission.Internal.UTF8 as UTF8
import           Fission.Security       (Digestable (..))
import           Fission.User.Role


-- DEFINITION


{-| This'll take our User model, defined in template Haskell,
    and translate them into data and entity types automatically.
-}
Database.generate [ Database.Types ]
  $( Database.entity "library/Fission/User/Model.entity" )



-- INSTANCES


instance Digestable UserId where
  digest = digest . UTF8.textShow


instance ToSchema UserId where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> NamedSchema (Just "UserID")
      |> pure
