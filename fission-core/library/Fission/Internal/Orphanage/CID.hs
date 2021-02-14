{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.CID () where

import           Data.Aeson
import qualified Data.Aeson.Types       as JSON


import           Network.IPFS.CID.Types
import           RIO

instance ToJSONKey CID where
  toJSONKey = JSON.toJSONKeyText textDisplay

instance FromJSONKey CID where
  fromJSONKey = FromJSONKeyValue parseJSON
