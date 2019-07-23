module Fission.User.CID.Selector where
--   ( userCID'
--   , userFK'
--   , cid'
--   , insertedAt'
--   , modifiedAt'
--   ) where

-- import Database.Selda

-- import Fission.User.CID.Table
-- import Fission.User.CID.Types
-- import Fission.User.Types

-- userCID'    :: Selector UserCID (ID UserCID)
-- userFK'     :: Selector UserCID (ID User)
-- cid'        :: Selector UserCID Text
-- insertedAt' :: Selector UserCID UTCTime
-- modifiedAt' :: Selector UserCID UTCTime

-- userCID' :*: userFK'
--          :*: cid'
--          :*: insertedAt'
--          :*: modifiedAt' = selectors userCIDs
