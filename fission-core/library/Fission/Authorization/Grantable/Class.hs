module Fission.Authorization.Grantable.Class (Grantable (..)) where

import           Fission.Prelude

import           Fission.Models
import           Fission.Ownership

import           Fission.User.Namespace.Class

import           Fission.WNFS.Privilege.Types                 as Subgraph
-- import           Fission.WNFS.Subgraph.Types
import           Fission.Prelude

import           Fission.Error.UserNotAuthorized.Types

import           Fission.Models
import           Fission.URL.Types

import           Fission.Authorization.Access.Unchecked.Types
import           Fission.User.Namespace.Class

import           Fission.WNFS.Permission.Types                as Subgraph
import           Fission.WNFS.Privilege.Types                 as Subgraph
import           Fission.WNFS.Subgraph.Types                  as Subgraph

import           Fission.App.Permission.Types                 as App
import qualified Fission.App.Permission.Types                 as App.Permission

import           Fission.App.Privilege.Types                  as App
import qualified Fission.App.Privilege.Types                  as App.Privilege

import           Fission.Domain.Permission.Types              as Domain
import           Fission.Domain.Privilege.Types               as Domain

import           Fission.Authorization.Access.Unchecked.Types
import           Fission.Authorization.Allowable.Class

import           Fission.Models
import           Fission.URL.Types

import           Fission.Authorization.Access.Unchecked.Types
import           Fission.User.Namespace.Class

import           Fission.WNFS.Permission.Types                as Subgraph
import           Fission.WNFS.Privilege.Types                 as Subgraph
import           Fission.WNFS.Subgraph.Types                  as Subgraph

import           Fission.App.Permission.Types                 as App
import           Fission.App.Privilege.Types                  as App
import qualified Fission.App.Retriever.Class                  as App

import           Fission.Domain.Privilege.Types               as Domain
import qualified Fission.Domain.Retriever.Class               as Domain

import qualified Fission.App.Domain.Retriever.Class           as AppDomain

class (Monad m, Allowable resource) => Grantable resource m where
  -- | Lookup and veriy that the unchecked privilege is valid
  --   Returns a proof to carry around on success
  grant ::
       ActionScope resource             -- ^ Requested scope of action
    -> Unchecked (ActionScope resource) -- ^ Unchecked permission
    -> m (Either (UserNotAuthorized resource) (Access resource))

instance
  ( App.Retriever       m
  , Domain.Retriever    m
  , AppDomain.Retriever m
  )
  => Grantable App m where
    grant requested = \case
      AsUser user ->
        checkApp requested user

      privilege `DelegatedBy` user@(Entity userId _) ->
        case relationship requested privilege of
          Sibling  -> return . Left $ UserNotAuthorized userId
          Ancestor -> return . Left $ UserNotAuthorized userId
          _        -> checkApp requested user

checkApp ::
  ( App.Retriever       m
  , Domain.Retriever    m
  , AppDomain.Retriever m
  )
  => App.Privilege
  -> Entity User
  -> m (Either (UserNotAuthorized App) (Access App))
checkApp App.Privilege {url = url@URL {..}, capability} (Entity userId _) =
  AppDomain.primarySibling userId url >>= \case
    Left _ ->
      return . Left $ UserNotAuthorized userId

    Right appDomain@(Entity _ AppDomain {appDomainAppId, appDomainDomainName, appDomainSubdomain}) ->
      -- NOTE App.byId also does access check
      App.byId userId appDomainAppId >>= \case
        Left _ ->
          return . Left $ UserNotAuthorized userId

        Right app ->
          return $ Right App.Permission
            { app
            , appDomain
            , capability
            }

instance Domain.Retriever m => Grantable Domain m where
  grant requested = \case
    AsUser user ->
      checkDomain requested user

    privilege `DelegatedBy` user@(Entity userId _) ->
      case relationship requested privilege of
        Sibling  -> return . Left $ UserNotAuthorized userId
        Ancestor -> return . Left $ UserNotAuthorized userId
        _        -> checkDomain requested user

checkDomain ::
  Domain.Retriever m
  => Domain.Privilege
  -> Entity User
  -> m (Either (UserNotAuthorized Domain) (Access Domain))
checkDomain Domain.Privilege {..} (Entity userId _) =
  Domain.getByDomainName domainName >>= \case
    Left _ ->
      return . Left $ UserNotAuthorized userId

    Right domain ->
      if domain `isOwnedBy` userId
        then return $ Right Domain.Permission {domain, capability}
        else return . Left $ UserNotAuthorized userId

instance MonadUserNamespace m => Grantable Subgraph m where
  grant requested = \case
    AsUser user ->
      checkSubgraph requested user

    privilege `DelegatedBy` user@(Entity userId _) ->
      case relationship requested privilege of
        Sibling  -> return . Left $ UserNotAuthorized userId
        Ancestor -> return . Left $ UserNotAuthorized userId
        _        -> checkSubgraph requested user

checkSubgraph ::
  MonadUserNamespace m
  => Subgraph.Privilege
  -> Entity User
  -> m (Either (UserNotAuthorized Subgraph) (Access Subgraph))
checkSubgraph privilege@Subgraph.Privilege {subgraph = Subgraph {..}} owner@(Entity userId User {..}) = do
  fissionNamespace <- getUserNamespace
  if namespace == fissionNamespace && username == userUsername
     then return $ Right Subgraph.Permission {owner, privilege}
     else return . Left $ UserNotAuthorized userId
