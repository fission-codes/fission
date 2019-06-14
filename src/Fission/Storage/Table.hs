module Fission.Storage.Table
  ( lensPrefixed
  , Name (..)
  ) where

import           RIO
import qualified RIO.Partial as Partial
import           RIO.Text    (stripPrefix)

import Database.Selda

newtype Name a = Name { name :: TableName }
  deriving ( Show
           , Eq
           , IsString
           )

lensPrefixed :: Relational r => TableName -> [Attr r] -> Table r
lensPrefixed tableName conf =
  tableFieldMod tableName conf (Partial.fromJust . stripPrefix "_")
