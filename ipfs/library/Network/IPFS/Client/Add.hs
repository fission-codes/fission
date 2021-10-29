module Network.IPFS.Client.Add (API) where

import qualified RIO.ByteString.Lazy    as Lazy

import           Servant.API

import           Network.IPFS.CID.Types (CID)

type API = ReqBody '[PlainText] Lazy.ByteString
        :> Post    '[PlainText] CID
