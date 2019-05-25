{-# LANGUAGE NoImplicitPrelude #-}

module Fission.User.Selector
  ( id
  , role
  , herokuAddOnId
  , insertedAt
  , modifiedAt
  ) where

import RIO hiding (id)

import Database.Selda

import qualified Fission.Platform.Heroku as Heroku
import           Fission.User            (User, users)
import           Fission.User.Role       (Role)

id            :: Selector User (ID User)
role          :: Selector User Role
herokuAddOnId :: Selector User (Maybe (ID Heroku.AddOn))
insertedAt    :: Selector User UTCTime
modifiedAt    :: Selector User UTCTime

id :*: role
   :*: herokuAddOnId
   :*: insertedAt
   :*: modifiedAt = selectors users
