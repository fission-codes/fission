module Fission.App.Domain.Types (IsBare (..)) where

import           Database.Persist.Sql

import           Fission.Prelude

-- | Hack to work around nullable uniqueness constraints (DB-specific)
data IsBare = IsBare
  deriving (Show, Eq)

instance PersistField IsBare where
  toPersistValue IsBare = PersistBool True

  fromPersistValue (PersistBool True) = Right IsBare
  fromPersistValue _                  = Left "Not App.Domain.IsBare"

instance PersistFieldSql IsBare where
  sqlType _ = SqlBool
