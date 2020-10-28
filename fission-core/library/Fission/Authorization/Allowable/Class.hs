-- {-# LANGUAGE AllowAmbiguousTypes #-}

 module Fission.Authorization.Allowable.Class (Allowable (..)) where

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

class Allowable resource where
  type ActionScope resource = s | s -> resource -- ^ Scope of action to (e.g. requested privilege)
  type Access      resource = a | a -> resource -- ^ Validation proof (to be cached)

  -- | Check if a requested privilege is granted by the Access proof
  isAllowed :: ActionScope resource -> Access resource -> Bool

instance Allowable App where
  type ActionScope App = App.Privilege
  type Access      App = App.Permission

  isAllowed priv@App.Privilege {url} perm@App.Permission {appDomain = Entity _ AppDomain {..}} =
       url `isEncompassedBy` URL appDomainDomainName appDomainSubdomain
    && App.Privilege.capability priv `isEncompassedBy` App.Permission.capability perm

instance Allowable Domain where
  type ActionScope Domain = Domain.Privilege
  type Access      Domain = Domain.Permission

  isAllowed Domain.Privilege {capability = privCap, ..} perm@Domain.Permission {capability = permCap} =
       privCap `isEncompassedBy` permCap
    && domainName == (perm |> Domain.domain |> domainDomainName)

instance Allowable Subgraph where
  type ActionScope Subgraph = Subgraph.Privilege
  type Access      Subgraph = Subgraph.Permission

  isAllowed priv perm =
       priv `isEncompassedBy` privilege perm
    && priv `isEncompassedBy` privilege perm
