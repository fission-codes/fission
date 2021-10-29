module Network.IPFS.DAG.Node.Types (Node (..)) where

import           Network.IPFS.Prelude

import           Data.Vector
import           Network.IPFS.DAG.Link.Types as DAG


data Node = Node
  { dataBlock :: Text
  , links     :: [DAG.Link]
  } deriving (Show, Eq)

instance ToJSON Node where
  toJSON (Node dataBlock links) =
    Object [ ("data", String dataBlock)
           , ("links", Array . fromList $ fmap toJSON links)
           ]

