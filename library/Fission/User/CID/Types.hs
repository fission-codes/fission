module Fission.User.CID.Types

import Fission.Prelude

-- Database

import           Fission.Storage.Persist (Generate(..))
import qualified Fission.Storage.Persist as Persist


{-| This'll take our CID model, defined in template Haskell,
    and translate them into data and entity types automatically.
-}
Persist.generate
  [ Types ]
  [ "library/Fission/User/CID/Model.entity" ]
