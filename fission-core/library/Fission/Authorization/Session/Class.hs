module Fission.Authorization.Session.Class
  ( MonadAuthSession     (..)
  , MonadLiftAuthSession (..)
  ) where

import           Fission.Prelude

import           Fission.Error.UserNotAuthorized.Types
import           Fission.Models

import           Fission.Authorization.Allowable
import           Fission.Authorization.Grantable

import           Fission.Authorization.Session.Types   as Authorization

class Grantable resource m => MonadAuthSession resource m where
  addAccess  :: Access resource -> m ()
  allChecked :: m [Access resource]

  dropUnchecked :: ActionScope resource -> UserId -> m ()
  allUnchecked  :: m [Unchecked (ActionScope resource)]

-- Easier than refactoring Fission to FissionT right now ~@expede
class (Monad m, Monad n) => MonadLiftAuthSession n m | m -> n where
  withAuthSession :: Authorization.Session -> n a -> m a
