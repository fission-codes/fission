module Fission.User.Types (User (..))where

import Data.Swagger
import Fission.Prelude

-- Database

import           Fission.Storage.Persist (Generate(..))
import qualified Fission.Storage.Persist as Persist

-- User instances

import qualified Fission.Internal.UTF8 as UTF8
import           Fission.Security       (Digestable (..))
import           Fission.Security.Types (SecretDigest)
import           Fission.User.Role


{-| This'll take our User model, defined in template Haskell,
    and translate them into data and entity types automatically.
-}
Persist.generate
  [ Types ]
  [ "library/Fission/User/Model" ]

-- TODO: Not sure what to do with this yet.
--
-- instance Digestable (ID User) where
--   digest = digest . UTF8.textShow
--
-- instance ToSchema (ID User) where
--   declareNamedSchema _ =
--     mempty
--       |> type_ ?~ SwaggerInteger
--       |> NamedSchema (Just "UserID")
--       |> pure
