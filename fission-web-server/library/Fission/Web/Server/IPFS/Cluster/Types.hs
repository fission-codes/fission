module Fission.Web.Server.IPFS.Cluster.Types ( parseResp ) where

import           Fission.Prelude
import           Servant.Client

import           Fission.Internal.Orphanage.ClientError      ()
import           Fission.Web.Server.IPFS.Cluster.Error.Types

data StatusResp = StatusResp
  { cid     :: IPFS.CID
  , peerMap :: [peerStatus]
  }

instance FromJSON StatusResp where
  parseJSON = withObject "status resp" \obj -> do
    cid <- obj .: "cid" .: "/"
    peerMap <- parseEntry . toList obj.: "peerMap"

data PeerStatus = PeerStatus
  { peer      :: Text
  , peerName  :: Text
  , status    :: Text
  , timestamp :: Text
  , error     :: Text
  }

instance FromJSON PeerStatus where
  parseJSON = withObject "peer map" \obj -> do
    peer <- obj .: "peer"
    peerName <- obj .: "peername"
    status <- obj .: "status"
    timestamp <- obj .: "timestamp"
    error <- obj .: "error"


parseResp :: Text -> StatusResp
parseResp = parseJSON
