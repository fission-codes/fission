module Fission.Domain.Subdomain.Types (Subdomain (..)) where

import qualified RIO.Text as Text

import           Data.Swagger
import           Database.Persist.Postgresql

import           Fission.Prelude

newtype Subdomain = Subdomain { getSubdomain :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToJSON
                    , FromJSON
                    , ToSchema
                    )

instance PersistField Subdomain where
  toPersistValue (Subdomain subdomain') = PersistText subdomain'
  fromPersistValue = \case
    PersistText subdomain' -> Right (Subdomain subdomain')
    other -> Left ("Invalid Persistent App Subdomain: " <> Text.pack (show other))

instance PersistFieldSql Subdomain where
  sqlType _pxy = SqlString
