module Fission.User.CID.Table where
--   ( name
--   , userCIDs
--   ) where

-- import Database.Selda

-- import           Fission.User.CID.Types
-- import qualified Fission.Storage.Table as Table

-- -- | The name of the 'users' table
-- name :: Table.Name UserCID
-- name = "user_cids"

-- -- | The 'User' table
-- userCIDs :: Table UserCID
-- userCIDs = Table.lensPrefixed (Table.name name)
--   [ #_userCID :- autoPrimary
--   , #_userFK  :- index
--   , #_cid     :- index
--   ]
