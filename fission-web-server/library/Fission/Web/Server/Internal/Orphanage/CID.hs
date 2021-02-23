{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.Server.Internal.Orphanage.CID () where

import           Data.Aeson

import           Database.Persist.Class
import           Database.Persist.Sql

import           Network.IPFS.CID.Types

import           RIO
import qualified RIO.Text                       as Text

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

instance FromJSONKey CID where
  fromJSONKey = FromJSONKeyText \txt -> CID txt
