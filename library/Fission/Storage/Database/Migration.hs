module Fission.Storage.Database.Migration where

-- Fission

import qualified Fission.Storage.Database as Database
import           Fission.Model


{-| This'll take all our models, defined in template Haskell,
    and translate them into migrations automatically.
-}
-- TODO
--
-- Database.generate
--   [ Database.Migrations ]
--   $( Database.entities
--     [ "library/Fission/Platform/Heroku/AddOn/Model.entity"
--     , "library/Fission/User/CID/Model.entity"
--     , "library/Fission/User/Model.entity"
--     ]
--   )
