-- | Table creation and migration sequences
module Fission.Storage.PostgreSQL.UpgradeUser
  ( upgrade
  ) where

import RIO

import Database.Selda.Migrations
import Database.Selda.Backend.Internal

import Database.Selda

import qualified Fission.User.Table  as User.Table
import           Fission.User.History.User0

import Fission.User.Selector


upgrade :: (MonadMask m, MonadSelda m, Functor (Col (Backend m))) => m ()
upgrade = migrate user0Table User.Table.users \row -> new
  [ userID'        := toId . fromId <$> row ! #_userID
  , username'      := text "username" -- TODO: hashMe <$> row ! userID''
  , email'         := null_
  , role'          := row ! #_role
  , active'        := row ! #_active
  , herokuAddOnID' := row ! #_herokuAddOnID
  , secretDigest'  := row ! #_secretDigest -- password field already?
  , insertedAt'    := row ! #_insertedAt
  , modifiedAt'    := row ! #_modifiedAt
  ]