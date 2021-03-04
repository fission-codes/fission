-- | Avoid when possible. Sometimes required to embed a String as SomeException
module Fission.Error.GenericError.Types (GenericError (..)) where

import           Fission.Prelude

newtype GenericError = GenericError String
  deriving stock    (Eq, Show)
  deriving anyclass (Exception)
