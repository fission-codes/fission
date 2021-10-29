module Network.IPFS.Client.Cat (API) where

import           Servant.API

import qualified Network.IPFS.Client.Param            as Param
import qualified Network.IPFS.File.Types              as File
import           Network.IPFS.MIME.RawPlainText.Types

type API = Param.CID'
        :> Post '[RawPlainText] File.Serialized
