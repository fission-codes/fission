module Fission.Error.Class
  ( SuperError (..)
  , embed
  ) where

import           RIO

import qualified Data.Bifunctor as BF

-- | Abstract over lifting a suberror into a parent error type
class SuperError childErr parentErr where
  toError :: childErr -> parentErr

instance SuperError err err where
  toError = id

-- | Switch context from some leaf errors to the trunk error type
embed :: SuperError childErr parentErr => Either childErr a -> Either parentErr a
embed = BF.first toError
