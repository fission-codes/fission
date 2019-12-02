{-| Some niceties regarding the persistent database library.
    This will generate snake case database names (tables, columns, etc).
    For example, `IpfsPeers` would become `ipfs_peers`.

-}
module Fission.Storage.Persist

import qualified Database.Persist.Quasi as Persist
import qualified Language.Haskell.TH.Syntax as TemplateHaskell

-- Fission

import Fission.Prelude


{-| A sum type representing what code can be generated
    using the persistent library.
-}
type Generate = Migrations | Types


{-| `Persist.share` with our `Generation` type.
-}
generate :: [ Generate ] -> [ FilePath ] -> TemplateHaskell.Q TemplateHaskell.Exp
generate toGenerate filePaths =
  filePaths
    |> Persist.persistManyFileWith
        Persist.lowerCaseSettings
    |> Persist.share
        (
          map
            (\g ->
              case g of
                Migrations -> mkMigrate "migrateAll"
                Types -> mkPersist Persist.sqlSettings
            )
            toGenerate
        )


{-| Alias for `derivePersistField`.

    This generates instances for `PersistField`,
    defining the `toPersistValue` and `fromPersistValue` methods.

-}
generateInstances =
  Persist.derivePersistField
