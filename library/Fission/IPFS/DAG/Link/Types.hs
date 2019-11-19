module Fission.IPFS.DAG.Link.Types (Link (..)) where

import Fission.Prelude
import Fission.IPFS.Types as IPFS
import Data.Text as T


data Link = Link
  { cid  :: IPFS.CID
  , name :: IPFS.Name
  , size :: Integer
  } deriving (Show, Eq, Generic)


instance ToJSON Link where
  toJSON (Link cid name size) =
    Object [ ("Name", String . T.pack <| unName name)
           , ("Size", Number  <| fromIntegral size)
           , ("Cid", Object [("/", String <| unaddress cid)])
           ]

