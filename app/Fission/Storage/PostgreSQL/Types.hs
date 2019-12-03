module Fission.Storage.PostgreSQL.Types

import Fission.Prelude


data ConnectionInfo = ConnectionInfo
  { database :: Text
  , host :: Text
  , password :: Maybe Text
  , port :: Int
  , username :: Maybe Text
  }


instance FromJSON ConnectionInfo where
  parseJSON = withObject "ConnectionInfo" \obj -> do
    database  <- obj .:  "database"
    host      <- obj .:  "host"
    password  <- obj .:? "password"
    port      <- obj .:? "port" .!= 5432
    username  <- obj .:? "username"

    return ConnectionInfo {..}
