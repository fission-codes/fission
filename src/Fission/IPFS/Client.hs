module Fission.IPFS.Client
  ( API
  , cat
  , pin
  , unpin
  )where

import RIO

import qualified Network.HTTP.Client as HTTP
import           Servant
import qualified Servant.Client.Streaming as Streaming

import qualified Fission.File.Types      as File
import qualified Fission.IPFS.Client.Cat as Cat
import qualified Fission.IPFS.Client.Pin as Pin

type API
  = "api"
  :> "v0"
  :> V0API

type V0API = "cat" :> Cat.API
        :<|> "pin" :> Pin.API

cat ::   Text        -> Streaming.ClientM File.Serialized
pin ::   Text        -> Streaming.ClientM Text
unpin :: Text -> Bool -> Streaming.ClientM Text
cat :<|> pin :<|> unpin = Streaming.client (Proxy :: Proxy API)

-- env manager = Streaming.clientEnv manager "localhost:5001"

run = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  Streaming.runClientM (Streaming.mkClientEnv manager)
