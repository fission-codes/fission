module Fission.IPFS.Client.Add (API) where

import Servant

import           Fission.IPFS.CID.Types (CID)
import qualified Fission.IPFS.Client.Param as Param
import qualified Fission.File.Types        as File

type API = ByteString :> Post '[PlainText] CID
