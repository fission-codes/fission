module Fission.Web.Server.User.Role.Types (Role (..)) where

import           Database.Persist.Sql
import qualified RIO.Text             as Text

import           Fission.Prelude

data Role
  = Regular
  | Admin
  deriving ( Show
           , Read
           , Eq
           )

instance PersistField Role where
  toPersistValue role =
    PersistText . Text.pack $ show role

  fromPersistValue val =
    case fromPersistValueText val of
      Right "Regular" -> Right Regular
      Right "Admin"   -> Right Admin
      Right _         -> Left "Not a valid Role value"
      Left err        -> Left err

instance PersistFieldSql Role where
  sqlType _ = SqlString
