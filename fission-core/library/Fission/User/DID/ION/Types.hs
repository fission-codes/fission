module Fission.User.DID.ION.Types (ID (..)) where

import           Database.Persist.Sql
import           Servant.API

import           Fission.Prelude

newtype ID = ID { canonicalID :: Text }
  deriving (Eq, Show)
  deriving newtype ( Arbitrary
                   , Display
                   , ToJSON
                   , FromJSON
                   , ToHttpApiData
                   , FromHttpApiData
                   , PersistField
                   , PersistFieldSql
                   )
