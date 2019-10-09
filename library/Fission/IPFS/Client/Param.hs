module Fission.IPFS.Client.Param
  ( CID
  , IsRecursive
  ) where

import RIO
import Servant

type CID = QueryParam' '[Required, Strict] "arg" Text
type IsRecursive = QueryFlag "recursive"
