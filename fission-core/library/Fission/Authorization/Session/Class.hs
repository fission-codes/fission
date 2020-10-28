-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Fission.Authorization.Session.Class (MonadAuthSession (..)) where

import           Fission.Prelude

import           Fission.Error.ActionNotAuthorized.Types
import           Fission.Models

import           Fission.Authorization.Allowable
import           Fission.Authorization.Grantable

import           Fission.Authorization.Session.Types

class Grantable resource m => MonadAuthSession resource m where
  addAccess  :: Access resource -> m ()
  allChecked :: m [Access resource]

  dropUnchecked :: ActionScope resource -> UserId -> m ()
  allUnchecked  :: m [Unchecked (ActionScope resource)]
