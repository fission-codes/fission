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

run :: NFData a => Streaming.ClientM a -> IO (Either Streaming.ServantError a)
run query = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  let env = (Streaming.mkClientEnv manager (Streaming.BaseUrl Streaming.Http "localhost" 8081 ""))
  Streaming.runClientM query env


{-
It allows using this module's ClientM in a direct style. The NFData constraint however prevents using this function with genuine streaming response types (SourceT, Conduit, pipes Proxy or Machine). For those you have to use withClientM.
-}
