module Fission.Web.Server.Ownership.Class (Owned (..)) where

import           Database.Esqueleto.Legacy

import           Fission.Web.Server.Models

class Owned item where
  ownerId :: item -> UserId

instance Owned model => Owned (Entity model) where
  ownerId (Entity _ model) = ownerId model

instance Owned (Entity User) where
  ownerId (Entity id _) = id

instance Owned App where
  ownerId = appOwnerId

instance Owned Domain where
  ownerId = domainOwnerId

instance Owned LoosePin where
  ownerId = loosePinOwnerId
