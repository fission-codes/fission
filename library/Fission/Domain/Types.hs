module Fission.Domain.Types
  ( Name (..)
  ) where

import qualified RIO.Text as Text

import           Data.Swagger
import           Database.Persist.Postgresql

import           Fission.Prelude

newtype Name = Name { unName :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToJSON
                    , FromJSON
                    , ToSchema
                    )

instance PersistField Name where
  toPersistValue (Name name') = PersistText name'
  fromPersistValue = \case
    PersistText name' -> Right (Name name')
    other             -> Left ("Invalid Persistent Domain Name: " <> Text.pack (show other))

instance PersistFieldSql Name where
  sqlType _pxy = SqlString
