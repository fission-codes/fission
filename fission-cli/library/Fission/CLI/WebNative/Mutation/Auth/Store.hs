module Fission.CLI.WebNative.Mutation.Auth.Store
  ( getBy
  , module Fission.CLI.WebNative.Mutation.Auth.Store.Class
  ) where

import qualified RIO.Set                                          as Set

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import qualified Fission.JSON                                     as JSON

import qualified Fission.Web.Auth.Token.Bearer.Types              as Bearer
import           Fission.Web.Auth.Token.JWT                       as JWT
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types       as UCAN

import           Fission.CLI.WebNative.Mutation.Auth.Store.Class

getBy ::
  ( MonadStore m
  , MonadRaise m
  , m `Raises` JSON.Error
  )
  => (UCAN.Resource -> Bool)
  -> m (Either (NotFound Bearer.Token) Bearer.Token)
getBy matcher = do
  bearerTokens <- getAll

  case Set.toList $ Set.filter normalizedMatcher bearerTokens of
    []           -> return $ Left NotFound
    (bearer : _) -> return $ Right bearer

  where
    normalizedMatcher :: Bearer.Token -> Bool
    normalizedMatcher Bearer.Token {jwt = JWT {claims = JWT.Claims {resource}}} =
      case resource of
        Nothing             -> False
        Just Complete       -> True
        Just (Subset inner) -> matcher inner
