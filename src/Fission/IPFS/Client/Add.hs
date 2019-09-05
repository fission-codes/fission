module Fission.IPFS.Client.Add (API) where

import Servant

import           Fission.IPFS.CID.Types (CID)
import qualified Fission.File.Types        as File

type API = ReqBody '[OctetStream] File.Serialized
        :> Post    '[PlainText]   CID
