{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fission.Internal.Orphanage.UUID () where

import qualified Data.ByteString.Char8 as BS8
import           Data.UUID             as UUID
import           Database.Persist.Sql

import           Fission.Prelude

instance PersistField UUID where
  toPersistValue uuid = PersistLiteralEscaped . BS8.pack $ UUID.toString uuid

  fromPersistValue (PersistLiteralEscaped txt) =
    case UUID.fromString $ BS8.unpack txt of
      Just x  -> Right x
      Nothing -> Left "Invalid UUID"

  fromPersistValue _ =
    Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"
