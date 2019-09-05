module Fission.IPFS.Client.Add (API) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Servant

import           Fission.IPFS.CID.Types (CID)
-- import qualified Fission.File.Types        as File

type API = ReqBody '[PlainText] Lazy.ByteString -- Text
        :> Post    '[PlainText] CID
