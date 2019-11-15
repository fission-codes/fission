module Fission.IPFS.DAG.Node.Types (Node (..)) where

import Fission.Prelude

import Data.Vector
import Fission.IPFS.DAG.Link.Types as DAG


data Node = Node
  { dataBlock :: Text
  , links :: [DAG.Link]
  }

instance ToJSON Node where
  toJSON (Node dataBlock links) = 
    Object [ ("data", String dataBlock)
           , ("links", Array . fromList <| toJSON <$> links)
           ]

