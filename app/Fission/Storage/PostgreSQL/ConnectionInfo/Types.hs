module Fission.Storage.PostgreSQL.ConnectionInfo.Types (ConnectionInfo (..)) where

import qualified RIO.List as List
import qualified RIO.Text as Text

import           Fission.Prelude

data ConnectionInfo = ConnectionInfo
  { pgDatabase :: Text
  , pgHost     :: Text
  , pgPort     :: Int
  , pgUsername :: Maybe Text
  , pgPassword :: Maybe Text
  }

instance Show ConnectionInfo where
  show ConnectionInfo {..} =
    [ "host="   <> Text.unpack pgHost
    , "port="   <> show pgPort
    , "dbname=" <> Text.unpack pgDatabase
    , maybeField "user"     pgUsername
    , maybeField "password" pgPassword
    ] |> List.intersperse " "
      |> mconcat
    where
      maybeField :: String -> Maybe Text -> String
      maybeField key = maybe "" \value ->
        value
          |> Text.unpack
          |> pgOpt key

      pgOpt :: String -> String -> String
      pgOpt key value = mconcat [key, "=", value]

instance FromJSON ConnectionInfo where
  parseJSON = withObject "ConnectionInfo" \obj -> do
    pgDatabase  <- obj .:  "database"
    pgHost      <- obj .:  "host"
    pgPassword  <- obj .:? "password"
    pgPort      <- obj .:? "port" .!= 5432
    pgUsername  <- obj .:? "username"

    return ConnectionInfo {..}
