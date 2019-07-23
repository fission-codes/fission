module Fission.User.CID.Types
  ( UserCIDT (..)
  , UserCID
  , ID
  ) where

import RIO

import Control.Lens  ((.~))
import Data.Swagger
import Database.Beam

import Data.Time.Clock

import Fission.User (UserT)

-- | A user account, most likely a developer
data UserCIDT f = UserCID
  { _ID         :: C f Int
  , _userFK     :: PrimaryKey UserT f
  , _cid        :: C f Text -- SqlType for CID was getting hairy
  , _insertedAt :: C f UTCTime
  , _modifiedAt :: C f UTCTime
  } deriving ( Generic
             , Beamable
             )

type UserCID = UserCIDT Identity
deriving instance Show UserCID

type ID = PrimaryKey UserCIDT Identity
deriving instance Show ID

instance Table UserCIDT where
  data PrimaryKey UserCIDT f = ID (C f Int)
    deriving (Generic, Beamable)

  primaryKey = ID . _ID

instance ToSchema ID where
  declareNamedSchema _ =
     return $ NamedSchema (Just "UserCID")
            $ mempty & type_ .~ SwaggerInteger
