module Network.IPFS.Info.Types (Info (..)) where

import Network.IPFS.Prelude
import Network.IPFS.Peer.Types

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
