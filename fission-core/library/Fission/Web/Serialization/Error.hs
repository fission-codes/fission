module Fission.Web.Serialization.Error (Error (..)) where

import           Fission.Prelude

newtype Error = DeserializationError Text
  deriving anyclass (Exception)
  deriving newtype  (Eq, Show, Display)
