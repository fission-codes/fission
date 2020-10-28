module Fission.Authorization.Session
  ( prove
  -- * Reexports
  , module           Fission.Authorization.Session.Class
  , module           Fission.Authorization.Session.Types
  ) where

import           Fission.Authorization.Session.Class
import           Fission.Authorization.Session.Types

prove ::
  MonadAuthSession resource m
  => ActionScope resource
  -> m (Either (ActionNotAuthorized resource) (Access resource))
prove requested = do
  permissions <- allChecked
  case find (isAllowed requested) permissions of
    Just match ->
      return $ Right match

    Nothing -> do
      unchecked <- allUnchecked
      results   <- sequence (grant requested <$> unchecked)

      case find isRight results of
        Just match -> do
          addAccess match -- i.e. add to cache
          return $ Right match

        Nothing ->
          return $ Left ActionNotAuthorized
