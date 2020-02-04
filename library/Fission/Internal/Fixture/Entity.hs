module Fission.Internal.Fixture.Entity (entity) where

import qualified Database.Persist as Database
import qualified Database.Persist.Sql as Database

entity :: Database.ToBackendKey Database.SqlBackend model => model -> Database.Entity model
entity model = Database.Entity
  { entityKey = Database.toSqlKey 0
  , entityVal = model
  }
