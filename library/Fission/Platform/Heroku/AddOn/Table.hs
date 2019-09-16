module Fission.Platform.Heroku.AddOn.Table
  ( addOns
  , name
  ) where

import Database.Selda (Table, Attr (..), autoPrimary)

-- import           Fission.Internal.Orphanage ()
import qualified Fission.Storage.Table         as Table

import Fission.Platform.Heroku.AddOn.Types

name :: Table.Name AddOn
name = "heroku_add_ons"

addOns :: Table AddOn
addOns = Table.lensPrefixed (Table.name name) [#_addOnID :- autoPrimary]
