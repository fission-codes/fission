module Fission.IPFS.Info.Types (Info (..)) where

import Fission.Prelude
import Fission.IPFS.Peer.Types

data Info = Info
  { id              :: Text
  , publicKey       :: Text
  , addresses       :: [Peer]
  , agentVersion    :: Text
  , protocolVersion :: Text
  } deriving (Show, Eq)

instance FromJSON Info where
  parseJSON = withObject "IPFS.Info" \obj -> do
    id              <- obj .: "ID"
    publicKey       <- obj .: "PublicKey"
    addresses       <- obj .: "Addresses"
    agentVersion    <- obj .: "AgentVersion"
    protocolVersion <- obj .: "ProtocolVersion"

    return Info {..}
