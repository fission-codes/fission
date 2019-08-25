module Fission.IPFS.Client.Param
  ( CID
  , IsRecursive
  ) where

import RIO
import Servant

type CID = QueryParam' '[Required, Strict] "cid" Text
type IsRecursive = QueryFlag "recursive"
