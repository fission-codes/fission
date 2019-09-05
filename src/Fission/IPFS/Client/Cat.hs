module Fission.IPFS.Client.Cat (API) where

import Servant

import qualified Fission.File.Types        as File
import qualified Fission.IPFS.Client.Param as Param

type API = Param.CID
        :> Get '[PlainText] File.Serialized
