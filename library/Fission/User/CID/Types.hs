{-# LANGUAGE NoDeriveAnyClass #-}
module Fission.User.CID.Types where

import           Fission.Prelude

-- Database

import qualified Fission.Storage.Database as Database

-- Model Dependencies

import           Fission.User.Types (UserId)


{-| This'll take our CID model, defined in template Haskell,
    and translate them into data and entity types automatically.
-}
Database.generate [ Database.Types ]
  $( Database.entity "library/Fission/User/CID/Model.entity" )
