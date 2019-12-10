{-# LANGUAGE NoDeriveAnyClass #-}
module Fission.Platform.Heroku.AddOn.Types where

import Fission.Prelude

-- Database

import qualified Fission.Storage.Database as Database

-- Model Dependencies

import Fission.Platform.Heroku.Types (Region (..))


-- MODEL


{-| This'll take our AddOn model, defined in template Haskell,
    and translate them into data and entity types automatically.
-}
Database.generate [ Database.Types ]
  $( Database.entity "library/Fission/Platform/Heroku/AddOn/Model.entity" )



-- INSTANCES


-- TODO?
-- instance FromJSON AddOn where
--   parseJSON = withObject "Heroku.AddOn" \obj -> do
--     region          <- obj .: "region"
--     uuid            <- obj .: "uuid"
--     insertedAt      <- obj .: "inserted_at"
--     modifiedAt      <- obj .: "modified_at"
--
--     return <| AddOn {..}
--
--
-- instance ToJSON AddOn where
--   toJSON AddOn {..} = object
--     [ "region"        .= region
--     , "uuid"          .= uuid
--     , "inserted_at"   .= insertedAt
--     , "modified_at"   .= modifiedAt
--     ]
