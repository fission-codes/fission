module Fission.User.CID.Types (UserCID (..)) where

import RIO

import Database.Selda

import Fission.User (User (..))

-- | A relationship of 'CID' to a 'User'
data UserCID = UserCID
  { userCID    :: ID UserCID
  , userFK     :: ID User
  , cid        :: Text -- SqlType for CID was getting hairy
  , insertedAt :: UTCTime
  , modifiedAt :: UTCTime
  } deriving ( Show
             , Eq
             , Generic
             , SqlRow
             )
