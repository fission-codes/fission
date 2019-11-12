module Fission.Platform.Heroku.AddOn.Table
  ( addOns
  , name
  ) where

import Database.Selda (Table, Attr (..), autoPrimary, table)

import qualified Fission.Storage.Table         as Table
import           Fission.Platform.Heroku.AddOn.Types

name :: Table.Name AddOn
name = "heroku_add_ons"

addOns :: Table AddOn
addOns = table (Table.name name) [#addOnID :- autoPrimary]
