module Fission.Platform.Heroku.AddOn.Types (AddOn (..)) where

import RIO

import Data.UUID
import Database.Selda

import Fission.Platform.Heroku.Types (Region (..))

data AddOn = AddOn
  { addOnID    :: ID AddOn
  , uuid       :: UUID
  , region     :: Maybe Region
  , insertedAt :: UTCTime
  , modifiedAt :: UTCTime
  } deriving ( Show
             , Eq
             , SqlRow
             , Generic
             )
