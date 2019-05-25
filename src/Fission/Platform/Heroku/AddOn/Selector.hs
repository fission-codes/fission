{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Platform.Heroku.AddOn.Selector
  ( id
  , uuid
  , region
  , insertedAt
  , modifiedAt
  ) where

import RIO hiding (id)

import Data.UUID
import Database.Selda

import Fission.Platform.Heroku.AddOn  (AddOn, addOns)
import Fission.Platform.Heroku.Region (Region)

id         :: Selector AddOn (ID AddOn)
uuid       :: Selector AddOn UUID
region     :: Selector AddOn (Maybe Region)
insertedAt :: Selector AddOn UTCTime
modifiedAt :: Selector AddOn UTCTime

id :*: uuid
   :*: region
   :*: insertedAt
   :*: modifiedAt = selectors addOns
