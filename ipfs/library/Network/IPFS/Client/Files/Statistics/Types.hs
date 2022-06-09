module Network.IPFS.Client.Files.Statistics.Types (CidAPI, CidResponse (..)) where

import           Servant.API

import           Network.IPFS.CID.Types
import           Network.IPFS.Prelude


type CidAPI
  = "stat"
    :> QueryParam' '[Required] "arg" Text -- path
    :> QueryParam' '[Required] "hash" Bool -- must be true
    :> Post '[JSON] CidResponse

newtype CidResponse = CidResponse CID
  deriving (Eq, Show)

instance Display CidResponse where
  textDisplay (CidResponse cid) = textDisplay cid

instance FromJSON CidResponse where
  parseJSON = withText "Stat Response" (return . CidResponse . mkCID)
