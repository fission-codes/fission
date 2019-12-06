{-# LANGUAGE NoDeriveAnyClass #-}
module Fission.User.CID.Types where

import           Fission.Prelude

-- Database

import           Fission.Storage.Persist (Generate(..))
import qualified Fission.Storage.Persist as Persist

-- Model Dependencies

import           Fission.User.Types (UserId)


{-| This'll take our CID model, defined in template Haskell,
    and translate them into data and entity types automatically.
-}
Persist.generate [ Types ]
  $( Persist.file "library/Fission/User/CID/Model.entity" )
