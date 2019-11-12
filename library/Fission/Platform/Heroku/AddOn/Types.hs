module Fission.Platform.Heroku.AddOn.Types (AddOn (..)) where

import Data.UUID
import Database.Selda

import Fission.Platform.Heroku.Types (Region (..))
import Fission.Prelude

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
