module Fission.Platform.Heroku.AddOn.Types (AddOn (..)) where

import RIO

import Data.UUID
import Database.Selda

-- import Fission.Internal.Orphanage ()
import Fission.Platform.Heroku.Types (Region (..))

data AddOn = AddOn
  { _addOnID    :: ID AddOn
  , _uuid       :: UUID
  , _region     :: Maybe Region
  , _insertedAt :: UTCTime
  , _modifiedAt :: UTCTime
  } deriving ( Show
             , Eq
             , SqlRow
             , Generic
             )
