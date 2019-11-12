module Fission.Storage.Table (Name (..)) where

import Database.Selda
import Fission.Prelude

newtype Name a = Name { name :: TableName }
  deriving         (Eq, Show)
  deriving newtype IsString
