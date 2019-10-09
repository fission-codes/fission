{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.PGConnectInfo () where

import RIO

import Data.Aeson
import Database.Selda.PostgreSQL

deriving instance Show PGConnectInfo

instance FromJSON PGConnectInfo where
  parseJSON = withObject "PGConnectInfo" \obj -> do
    pgDatabase <- obj .:  "database"

    pgHost     <- obj .:  "host"
    pgPort     <- obj .:? "port" .!= 5432

    pgSchema   <- obj .:? "schema"
    pgUsername <- obj .:? "username"
    pgPassword <- obj .:? "password"

    return PGConnectInfo {..}
