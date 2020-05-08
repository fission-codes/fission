{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fission.Internal.Orphanage.UUID () where

import           Data.UUID             as UUID
import qualified Data.ByteString.Char8 as BS8
import           Database.Persist.Sql

import           Fission.Prelude

instance PersistField UUID where
  toPersistValue uuid = PersistDbSpecific . BS8.pack $ UUID.toString uuid

  fromPersistValue (PersistDbSpecific txt) =
    case UUID.fromString $ BS8.unpack txt of
      Just x  -> Right x
      Nothing -> Left "Invalid UUID"

  fromPersistValue _ =
    Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"
