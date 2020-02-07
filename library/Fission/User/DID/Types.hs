module Fission.User.DID.Types (DID (..)) where

import qualified RIO.Text as Text

import           Data.Swagger
import           Database.Persist.Postgresql

import           Fission.Prelude

newtype DID = DID { unDID :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToJSON
                    , FromJSON
                    , ToSchema
                    )

instance PersistField DID where
  toPersistValue (DID pk) = PersistText pk
  fromPersistValue = \case
    PersistText pk -> Right (DID pk)
    other          -> Left ("Invalid Persistent DID: " <> Text.pack (show other))

instance PersistFieldSql DID where
  sqlType _pxy = SqlString
