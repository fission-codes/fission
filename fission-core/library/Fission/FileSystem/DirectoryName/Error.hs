-- | Directory name errors
module Fission.FileSystem.DirectoryName.Error (Invalid (..)) where

import           Fission.Prelude

data Invalid = Invalid
  deriving ( Show
           , Eq
           , Exception
           )

instance Display Invalid where
  display Invalid = "Invalid directory name -- must be alphanumeric separated with hyphens and not blocklisted"
