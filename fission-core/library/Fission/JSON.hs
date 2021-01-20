module Fission.JSON
  ( betterError
  , module Fission.JSON.Error
  ) where

import           Fission.Prelude

import qualified Fission.JSON.Error as JSON

-- Reexports

import           Fission.JSON.Error

betterError :: Either String a -> Either JSON.Error a
betterError (Left str)  = Left $ JSON.Error str
betterError (Right val) = Right val
