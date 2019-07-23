-- | Selectors for the 'addOns' table
module Fission.Platform.Heroku.AddOn.Selector where
  -- ( addOnID'
  -- , uuid'
  -- , region'
  -- , insertedAt'
  -- , modifiedAt'
  -- ) where

-- import RIO

-- import Data.UUID
-- import Database.Selda

-- import Fission.Platform.Heroku.Types (Region (..))

-- import Fission.Platform.Heroku.AddOn.Table
-- import Fission.Platform.Heroku.AddOn.Types

-- addOnID'    :: Selector AddOn (ID AddOn)
-- uuid'       :: Selector AddOn UUID
-- region'     :: Selector AddOn (Maybe Region)
-- insertedAt' :: Selector AddOn UTCTime
-- modifiedAt' :: Selector AddOn UTCTime

-- addOnID' :*: uuid'
--          :*: region'
--          :*: insertedAt'
--          :*: modifiedAt' = selectors addOns
