-- | File name errors
module Fission.FileSystem.FileName.Error (Invalid (..)) where

import           Fission.Prelude

data Invalid = Invalid
  deriving ( Show
           , Eq
           , Exception
           )

instance Display Invalid where
  display Invalid = "Invalid file name -- must be alphanumeric separated with hyphens and not blocklisted"
