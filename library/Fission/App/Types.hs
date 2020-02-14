module Fission.App.Types
  ( Name        (..)
  , Description (..)
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
    other -> Left ("Invalid Persistent App Name: " <> Text.pack (show other))

instance PersistFieldSql Name where
  sqlType _pxy = SqlString

newtype Description = Description { unDescription :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToJSON
                    , FromJSON
                    , ToSchema
                    )

instance PersistField Description where
  toPersistValue (Description description') = PersistText description'
  fromPersistValue = \case
    PersistText description' -> Right (Description description')
    other -> Left ("Invalid Persistent App Description: " <> Text.pack (show other))

instance PersistFieldSql Description where
  sqlType _pxy = SqlString
