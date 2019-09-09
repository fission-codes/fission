module Fission.User.CID.Types (UserCID (..)) where

import RIO

import Control.Lens   ((.~))
import Data.Swagger
import Database.Selda

import Fission.User (User (..))

-- | A relationship of 'CID' to a 'User'
data UserCID = UserCID
  { _userCID    :: ID UserCID
  , _userFK     :: ID User
  , _cid        :: Text -- SqlType for CID was getting hairy
  , _insertedAt :: UTCTime
  , _modifiedAt :: UTCTime
  } deriving ( Show
             , Eq
             , Generic
             , SqlRow
             )

instance ToSchema (ID UserCID) where
  declareNamedSchema _ =
     return $ NamedSchema (Just "UserCID")
            $ mempty & type_ .~ SwaggerInteger
