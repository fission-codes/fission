module Fission.Authorization.Session.Class (MonadAuthSession (..)) where

import           Fission.Prelude

import           Fission.Authorization.Session.Types
import           Fission.Error.ActionNotAuthorized.Types

class Grantable resource m => MonadAuthSession resource m where
  addAccess  :: Access resource -> m ()
  allChecked :: m [Access resource]

  dropUnchecked :: Unchecked (ActionScope resource) -> m ()
  allUnchecked  :: m [Unchecked (Access resource)]
