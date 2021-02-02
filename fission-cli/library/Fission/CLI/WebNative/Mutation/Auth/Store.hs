module Fission.CLI.WebNative.Mutation.Auth.Store
  ( getBy
  , module Fission.CLU.WebNative.Mutation.Auth.Store.Class
  ) where

import           Fission.Prelude

import           Fission.CLI.WebNative.Mutation.Auth.Store.Class

getBy :: MonadStore m => (UCAN.Resource -> Bool) -> m (Either (NotFound UCAN) UCAN)
getBy matcher = do
  ucans <- getAll
  case filter normalizedMatcher ucans of
    []         -> return $ Left NotFound
    (ucan : _) -> return $ Right ucan

  where
    normalizedMatcher :: JWT -> Bool
    normalizedMatcher JWT {claims = JWT.Claims {resource}} =
      case resource of
        Nothing             -> False
        Just Complete       -> True
        Just (Subset inner) -> matcher inner
