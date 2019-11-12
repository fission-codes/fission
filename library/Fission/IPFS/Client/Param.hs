module Fission.IPFS.Client.Param
  ( CID
  , IsRecursive
  ) where

import Servant
import Fission.Prelude

type CID = QueryParam' '[Required, Strict] "arg" Text
type IsRecursive = QueryFlag "recursive"
