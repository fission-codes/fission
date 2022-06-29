module Network.IPFS.Client.Files.Statistics.Types (API, Response (..)) where

import           Servant.API

import           Network.IPFS.CID.Types
import           Network.IPFS.Prelude


type API
  = "stat"
    :> QueryParam' '[Required] "arg" Text -- path
    :> Post '[JSON] Response

newtype Response = Response { cid :: CID }
  deriving (Eq, Show)

instance Display Response where
  textDisplay (Response cid) = textDisplay cid

instance FromJSON Response where
  parseJSON = withObject "IPFS.Files.Stat.Response" \obj -> do
    cid <- obj .: "Hash"
    return $ Response cid