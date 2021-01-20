module Fission.Web.Serialization.Error (Error (..)) where

import           Fission.Prelude

newtype Error = DeserializationError Text
  deriving newtype (Eq, Show, Display)
