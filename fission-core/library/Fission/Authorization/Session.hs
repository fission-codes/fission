module Fission.Authorization.Session
  ( prove
  -- * Reexports
  , module           Fission.Authorization.Session.Class
  , module           Fission.Authorization.Session.Trans
  , module           Fission.Authorization.Session.Types
  ) where

import qualified RIO.List                            as List

import           Fission.Prelude

import           Fission.Error

import           Fission.Authorization.Allowable
import           Fission.Authorization.Grantable

import           Fission.Authorization.Session.Class
import           Fission.Authorization.Session.Trans
import           Fission.Authorization.Session.Types

prove :: forall resource m .
  MonadAuthSession resource m
  => ActionScope resource
  -> m (Either (ActionNotAuthorized resource) (Access resource))
prove requested = do
  permissions <- allChecked

  case List.find (isAllowed requested) permissions of
    Just access ->
      return $ Right access

    Nothing -> do
      uncheckedList <- allUnchecked
      results       <- sequence (grant requested <$> uncheckedList)

      case List.find isRight results of
        Just (Right access) -> do
          addAccess access -- i.e. add to cache
          return $ Right access

        _ ->
          return $ Left ActionNotAuthorized
