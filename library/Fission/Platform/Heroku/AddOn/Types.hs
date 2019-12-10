{-# LANGUAGE NoDeriveAnyClass #-}
module Fission.Platform.Heroku.AddOn.Types (AddOn(..), AddOnId) where

import Fission.Prelude

-- Database

import qualified Fission.Storage.Database as Database

-- Model Dependencies

import Data.UUID (UUID)
import Fission.Internal.UUID ()
import Fission.Platform.Heroku.Types (Region (..))


-- MODEL


{-| This'll take our AddOn model, defined in template Haskell,
    and translate them into data and entity types automatically.
-}
Database.generate [ Database.Types ]
  $( Database.entity "library/Fission/Platform/Heroku/AddOn/Model.entity" )
