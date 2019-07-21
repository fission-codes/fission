module Fission.Platform.Heroku.AddOn.Types (AddOn (..)) where

import RIO

import Data.UUID

import Data.Time.Clock
import Database.Beam

import Control.Lens   ((.~))
import Data.Swagger

import Fission.Internal.Orphanage ()
import Fission.Platform.Heroku.Types (Region (..))

data AddOnT f = AddOn
  { _ID         :: C f Int
  , _uuid       :: C f UUID
  , _region     :: C f (Maybe Region)
  , _insertedAt :: C f UTCTime
  , _modifiedAt :: C f UTCTime
  } deriving ( Generic
             , Beamable
             )

type AddOn = AddOnT Identity
deriving instance Show AddOn

type ID = PrimaryKey AddOnT Identity
deriving instance Show ID

instance Table AddOnT where
  data PrimaryKey AddOnT f = ID (C f Int)
    deriving (Generic, Beamable)

  primaryKey = ID . _ID

instance ToSchema ID where
  declareNamedSchema _ =
     return $ NamedSchema (Just "UserID")
            $ mempty & type_ .~ SwaggerInteger
