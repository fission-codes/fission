module Fission.Storage.Migration where

-- Fission

import           Fission.Storage.Persist (Generate(..))
import qualified Fission.Storage.Persist as Persist
import           Fission.Model


{-| Models to generate migrations for.
-}
models = Persist.files
  [ "library/Fission/Platform/Heroku/AddOn/Model.entity"
  , "library/Fission/User/CID/Model.entity"
  , "library/Fission/User/Model.entity"
  ]


{-| This'll take all our models, defined in template Haskell,
    and translate them into migrations automatically.
-}
Persist.generate [ Migrations ] models
