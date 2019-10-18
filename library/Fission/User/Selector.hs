module Fission.User.Selector
  ( userID'
  , username'
  , email'
  , role'
  , active'
  , herokuAddOnId'
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
username'      :: Selector User Text
email'         :: Selector User (Maybe Text)
role'          :: Selector User Role
active'        :: Selector User Bool
herokuAddOnId' :: Selector User (Maybe (ID Heroku.AddOn))
secretDigest'  :: Selector User SecretDigest
insertedAt'    :: Selector User UTCTime
modifiedAt'    :: Selector User UTCTime

userID' :*: username'
        :*: email'
        :*: role'
        :*: active'
        :*: herokuAddOnId'
        :*: secretDigest'
        :*: insertedAt'
        :*: modifiedAt' = selectors users
