module Fission.User.Selector
  ( userID'
  , role'
  , active'
  , herokuAddOnID'
  , secretDigest'
  , insertedAt'
  , modifiedAt'
  ) where

import RIO

import Database.Selda
import Database.Selda.MakeSelectors

import qualified Fission.Platform.Heroku.AddOn.Types as Heroku
import           Fission.Security.Types              (SecretDigest)

import Fission.User.Role
import Fission.User.Table
import Fission.User.Types

userID'        :: Selector User (ID User)
role'          :: Selector User Role
active'        :: Selector User Bool
herokuAddOnID' :: Selector User (Maybe (ID Heroku.AddOn))
secretDigest'  :: Selector User SecretDigest
insertedAt'    :: Selector User UTCTime
modifiedAt'    :: Selector User UTCTime

userID' :*: role'
        :*: active'
        :*: herokuAddOnID'
        :*: secretDigest'
        :*: insertedAt'
        :*: modifiedAt' = selectors users
