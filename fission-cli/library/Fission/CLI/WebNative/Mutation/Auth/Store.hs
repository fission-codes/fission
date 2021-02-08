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

getBy :: forall m.
  ( MonadStore   m
  , JWT.Resolver m
  , MonadLogger  m
  , MonadRaise   m
  , m `Raises` JSON.Error
  )
  => DID
  -> (Scope UCAN.Resource -> Bool)
  -> m (Either (NotFound Bearer.Token) Bearer.Token)
getBy did matcher = do
  bearerTokens <- getAll

  case Set.toList $ Set.filter normalizedMatcher bearerTokens of
    []           -> return $ Left NotFound
    (bearer : _) -> return $ Right bearer

  where
    -- FIXME BROOKE notes for tomorrow
    -- Resolve root UCAN to a specific DID because of MonadWebAuth (i.e. need access to linked root UCAN)
    normalizedMatcher :: Bearer.Token -> m Bool
    normalizedMatcher Bearer.Token {jwt = jwt@JWT {claims = JWT.Claims {resource}}} = do
      JWT {claims = JWT.Claims {sender}} <- JWT.getRoot jwt
      if sender == did
        then
          case resource of
            Nothing    -> return False
            Just inner -> return $ matcher inner

        else
          return False
