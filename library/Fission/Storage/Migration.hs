module Fission.Storage.Migration

import           Fission.Storage.Persist (Generate(..))
import qualified Fission.Storage.Persist as Persist


{-| This'll take all our models, defined in template Haskell,
    and translate them into migrations automatically.
-}
Persist.generate
  [ Migrations ]
  [ "library/Fission/Platform/Heroku/Model"
  , "library/Fission/User/CID/Model"
  , "library/Fission/User/Model"
  ]
