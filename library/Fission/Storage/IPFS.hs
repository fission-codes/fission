module Fission.Storage.IPFS
  ( addRaw
  , addFile
  , get
  , pin
  , unpin
  , DAG.put
  ) where

import  Fission.Storage.IPFS.Add as Add
import  Fission.Storage.IPFS.Get as Get
import  Fission.Storage.IPFS.Pin as Pin
import  Fission.Storage.IPFS.DAG as DAG