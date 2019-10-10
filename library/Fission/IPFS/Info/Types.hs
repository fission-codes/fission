module Fission.IPFS.Info.Types
  ( Info (..)
  , id'
  , publicKey
  , addresses
  , agentVersion
  , protocolVersion
  ) where

import RIO

import Data.Aeson

import Control.Lens (makeLenses)
import Fission.IPFS.Peer.Types

data Info = Info
  { _id'             :: Text
  , _publicKey       :: Text
  , _addresses       :: [Peer]
  , _agentVersion    :: Text
  , _protocolVersion :: Text
  } deriving (Show, Eq)

makeLenses ''Info

instance FromJSON Info where
  parseJSON = withObject "IPFS.Info" \obj -> do
    _id'             <- obj .: "ID"
    _publicKey       <- obj .: "PublicKey"
    _addresses       <- obj .: "Addresses"
    _agentVersion    <- obj .: "AgentVersion"
    _protocolVersion <- obj .: "ProtocolVersion"

    return Info {..}
