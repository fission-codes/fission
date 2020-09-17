module Fission.StatusCode.Types (StatusCode (..)) where

import           Fission.Prelude


newtype StatusCode = StatusCode { unStatusCode :: Int }
  deriving newtype ( Show
                   , Eq
                   , Display
                   )

