module Fission.Platform.Heroku.AddOn.Types

import Data.UUID
import Fission.Prelude

-- Database

import           Fission.Storage.Persist (Generate(..))
import qualified Fission.Storage.Persist as Persist

-- Dependencies

import Fission.Platform.Heroku.Types (Region (..))


{-| This'll take our AddOn model, defined in template Haskell,
    and translate them into data and entity types automatically.
-}
Persist.generate
 [ Types ]
 [ "library/Fission/Platform/Heroku/AddOn/Model" ]
