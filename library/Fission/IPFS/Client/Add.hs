module Fission.IPFS.Client.Add (API) where

import qualified RIO.ByteString.Lazy as Lazy

import Servant

import Fission.IPFS.CID.Types (CID)

type API = ReqBody '[PlainText] Lazy.ByteString
        :> Post    '[PlainText] CID
