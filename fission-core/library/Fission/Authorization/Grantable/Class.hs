{-# LANGUAGE AllowAmbiguousTypes #-}

module Fission.Authorization.Grantable.Class (Grantable (..)) where

import           Fission.Prelude

import           Fission.Models
import           Fission.Ownership

import           Fission.User.Namespace.Class

import           Fission.WNFS.Privilege.Types                 as Subgraph
-- import           Fission.WNFS.Subgraph.Types
import           Fission.Prelude

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
       ActionScope resource
    -> Unchecked (ActionScope resource)
    -> m (Maybe (Access resource))

instance
  ( App.Retriever       m
  , Domain.Retriever    m
  , AppDomain.Retriever m
  )
  => Grantable App m where
    grant requested (AsUser user) =
      checkApp requested user

    grant requested (privilege `DelegatedBy` user) =
      case relationship requested privilege of
        Sibling  -> return Nothing
        Ancestor -> return Nothing
        _        -> checkApp requested user

checkApp ::
  ( App.Retriever       m
  , Domain.Retriever    m
  , AppDomain.Retriever m
  )
  => App.Privilege
  -> Entity User
  -> m (Maybe (Access App))
checkApp App.Privilege {url = url@URL {..}, capability} (Entity userId _) =
  AppDomain.primarySibling userId url >>= \case
    Left _ ->
      return Nothing

    Right appDomain@(Entity _ AppDomain {appDomainAppId, appDomainDomainName, appDomainSubdomain}) ->
      -- NOTE App.byId also does access check
      App.byId userId appDomainAppId >>= \case
        Left _ ->
          return Nothing

        Right app ->
          return $ Just App.Permission
            { app
            , appDomain
            , capability
            }

instance Domain.Retriever m => Grantable Domain m where
  grant requested (AsUser user) =
    checkDomain requested user

  grant requested (privilege `DelegatedBy` user) =
    case relationship requested privilege of
      Sibling  -> return Nothing
      Ancestor -> return Nothing
      _        -> checkDomain requested user

checkDomain ::
  Domain.Retriever m
  => Domain.Privilege
  -> Entity User
  -> m (Maybe (Access Domain))
checkDomain Domain.Privilege {..} (Entity userId _) =
  Domain.getByDomainName domainName >>= \case
    Left _ ->
      return Nothing

    Right domain ->
      if domain `isOwnedBy` userId
        then return $ Just Domain.Permission {domain, capability}
        else return Nothing

instance MonadUserNamespace m => Grantable Subgraph m where
  grant requested (AsUser user) =
    checkSubgraph requested user

  grant requested (privilege `DelegatedBy` user) =
    case relationship requested privilege of
      Sibling  -> return Nothing
      Ancestor -> return Nothing
      _        -> checkSubgraph requested user

checkSubgraph ::
  MonadUserNamespace m
  => Subgraph.Privilege
  -> Entity User
  -> m (Maybe (Access Subgraph))
checkSubgraph privilege@Subgraph.Privilege {subgraph = Subgraph {..}} owner@(Entity _ User {..}) = do
  fissionNamespace <- getUserNamespace
  if namespace == fissionNamespace && username == userUsername
    then return $ Just Subgraph.Permission {owner, privilege}
    else return Nothing
