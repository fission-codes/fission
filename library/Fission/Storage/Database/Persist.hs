{-| Some niceties regarding the persistent database library.
    This will generate snake case database names (tables, columns, etc).
    For example, `IpfsPeers` would become `ipfs_peers`.
-}
module Fission.Storage.Database.Persist (Generate(..), entity, entities, generate, generateInstances) where

import qualified Database.Persist as Persist
import qualified Database.Persist.Quasi as Persist
import qualified Database.Persist.TH as Persist
import qualified Language.Haskell.TH.Syntax as TH


-- Fission

import Fission.Prelude


{-| A sum type representing what code can be generated
    using the persistent library.
-}
data Generate = Migrations | Types


{-| `Persist.persistFileWith` with default settings.
-}
entity :: FilePath -> TH.Q TH.Exp
entity =
  Persist.persistFileWith
    Persist.lowerCaseSettings


{-| `Persist.persistManyFileWith` with default settings.
-}
entities :: [ FilePath ] -> TH.Q TH.Exp
entities =
  Persist.persistManyFileWith
    Persist.lowerCaseSettings


{-| `Persist.share` with our `Generation` type.
-}
generate :: [ Generate ] -> [ Persist.EntityDef ] -> TH.Q [ TH.Dec ]
generate toGenerate =
  Persist.share
      (
        map
          (\case
              Migrations -> Persist.mkMigrate "migrateAll"
              Types -> Persist.mkPersist Persist.sqlSettings
          )
          toGenerate
      )


{-| Alias for `derivePersistField`.

    This generates instances for `PersistField`,
    defining the `toPersistValue` and `fromPersistValue` methods.

-}
generateInstances =
  Persist.derivePersistField
