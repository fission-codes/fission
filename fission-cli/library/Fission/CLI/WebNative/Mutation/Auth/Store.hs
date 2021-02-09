module Fission.CLI.WebNative.Mutation.Auth.Store
  ( getBy
  , module Fission.CLI.WebNative.Mutation.Auth.Store.Class
  ) where

import qualified RIO.Map                                          as Map

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import           Fission.User.DID.Types

import           Fission.Web.Auth.Token.JWT                       as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver              as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver              as JWT.Resolver

import qualified Fission.Web.Auth.Token.Bearer.Types              as Bearer
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types       as UCAN

import           Fission.CLI.WebNative.Mutation.Auth.Store.Class

getBy :: forall m.
  ( MonadStore   m
  , JWT.Resolver m
  , MonadLogger  m
  , MonadRaise   m
  , m `Raises` JWT.Resolver.Error
  )
  => DID
  -> (Scope UCAN.Resource -> Bool)
  -> m (Either (NotFound Bearer.Token) Bearer.Token)
getBy did matcher = do
  bearerTokens <- getAll

  filterM normalizedMatcher (Map.elems bearerTokens) >>= \case
    []           -> return $ Left NotFound
    (bearer : _) -> return $ Right bearer

  where
    normalizedMatcher :: Bearer.Token -> m Bool
    normalizedMatcher Bearer.Token {jwt = jwt@JWT {claims = JWT.Claims {resource}}} = do
      JWT {claims = JWT.Claims {sender}} <- ensureM $ JWT.getRoot jwt
      if sender == did
        then
          case resource of
            Nothing    -> return False
            Just inner -> return $ matcher inner

        else
          return False
