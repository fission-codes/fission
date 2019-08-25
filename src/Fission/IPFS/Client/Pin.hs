module Fission.IPFS.Client.Pin
  ( API
  , AddAPI
  , RemoveAPI
  ) where

import RIO

import Servant

import qualified Fission.IPFS.Client.Param as Param

type API = AddAPI :<|> RemoveAPI

type AddAPI
  = "add"
    :> Param.CID
    :> Put '[JSON] Text -- Not actually Text! Just for testing!

type RemoveAPI
  = "rm"
    :> Param.CID
    :> Param.IsRecursive
    :> Delete '[JSON] Text -- Not actually Text! Just for testing!
