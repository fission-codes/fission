module Fission.User.Types
  ( ID
  , User
  , UserT (..)
  ) where

import RIO

import Control.Lens   ((.~))
import Data.Swagger
-- import Database.Selda

import Data.Time.Clock
import Database.Beam

import qualified Fission.Platform.Heroku.AddOn as Heroku

import Fission.Security       (Digestable (..))
import Fission.Security.Types (SecretDigest)
import Fission.User.Role

import           Fission.Internal.Orphanage ()
import qualified Fission.Internal.UTF8      as UTF8

-- | A user account, most likely a developer
data UserT f = User
  { _ID            :: C f Int
  , _role          :: C f Role
  , _active        :: C f Bool
  , _herokuAddOnId :: PrimaryKey Heroku.AddOnT (Nullable f)
  , _secretDigest  :: C f SecretDigest
  , _insertedAt    :: C f UTCTime
  , _modifiedAt    :: C f UTCTime
  } deriving ( Generic
             , Beamable
             )

type User = UserT Identity
deriving instance Show User

type ID = PrimaryKey UserT Identity
deriving instance Show ID

instance Table UserT where
  data PrimaryKey UserT f = ID (C f Int)
    deriving (Generic, Beamable)

  primaryKey = ID . _ID

instance Digestable ID where
  digest = digest . UTF8.textShow

instance ToSchema ID where
  declareNamedSchema _ =
     return $ NamedSchema (Just "UserID")
            $ mempty & type_ .~ SwaggerInteger
