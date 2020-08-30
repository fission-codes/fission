{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Bytes () where

import           Fission.Prelude

import           Network.IPFS.Bytes.Types

import           Database.Persist.Class
import           Database.Persist.Types
import           Database.Persist.Sql


instance PersistField Bytes where
  toPersistValue (Bytes bytes') = PersistInt64 $ fromIntegral bytes'
  fromPersistValue = \case
    PersistInt64 bytes' -> 
      if bytes' >= 0
        then Right . Bytes $ fromIntegral bytes'
        else Left ("Invalid Persistent Bytes: " <> textShow bytes')

    other -> 
      Left ("Invalid Persistent Bytes: " <> textShow other)

instance PersistFieldSql Bytes where
  sqlType _pxy = SqlInt64
