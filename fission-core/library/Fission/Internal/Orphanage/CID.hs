{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.CID () where

import           Database.Persist.Class
import           Database.Persist.Types
import           Database.Persist.Sql

import           RIO
import qualified RIO.Text as Text
import           Network.IPFS.CID.Types

import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

instance Arbitrary CID where
  arbitrary = CID <$> arbitrary

instance PersistField CID where
  toPersistValue (CID hash) = PersistText hash
  fromPersistValue = \case
    PersistText hash -> Right (CID hash)
    other            -> Left ("Invalid Persistent CID: " <> Text.pack (show other))

instance PersistFieldSql CID where
  sqlType _pxy = SqlString
