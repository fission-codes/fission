module Fission.CLI.WebNative.Mutation.Auth.Store
  ( getBy
  , module Fission.CLI.WebNative.Mutation.Auth.Store.Class
  ) where

import           Fission.Prelude

import           Fission.CLI.WebNative.Mutation.Auth.Store.Class
import           Fission.Web.Auth.Token.JWT                       as JWT
import           Fission.Web.Auth.Token.JWT                       as UCAN
import           Fission.Web.Auth.Token.UCAN.Resource.Types       as UCAN

import           Fission.Error.NotFound.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

getBy :: MonadStore m => (UCAN.Resource -> Bool) -> m (Either (NotFound JWT) JWT)
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
