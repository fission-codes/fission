module Fission.Storage.IPFSClient where

import RIO
import qualified RIO.ByteString.Lazy as Lazy

import Servant
import Servant.Client
import qualified Servant.Client.Streaming as S

import Fission.File.Types as File
import Fission.Internal.Orphanage ()

type CIDArg = QueryParam' '[Required, Strict] "cid" Text

type RecursiveFlag = QueryFlag "recursive"

type CatAPI = CIDArg :> Get '[PlainText] File.Serialized

catApi :: Proxy CatAPI
catApi = Proxy

type PinAddAPI = "add"
                   :> CIDArg
                   :> Put '[JSON] Text -- Not actually Text! Just for testing!

type PinRmAPI = "rm"
                  :> CIDArg
                  :> RecursiveFlag
                  :> Delete '[JSON] Text -- Not actually Text! Just for testing!

type PinAPI = PinAddAPI :<|> PinRmAPI

type API = "cat" :> CatAPI
      :<|> "pin" :> PinAPI

type NestedAPI = "api" :> "v0" :> API

api :: Proxy API
api = Proxy

pinRm ::  Text -> Bool -> ClientM Text
pinAdd :: Text ->        ClientM Text
cat ::    Text ->        ClientM Serialized

cat :<|> (pinAdd :<|> pinRm) = client api
