module Fission.IPFS.Client
  ( API
  , cat
  , pin
  , run
  , unpin
  )where

import RIO

import Data.Has

import qualified Network.HTTP.Client as HTTP
import           Servant
import qualified Servant.Client.Streaming as Streaming

import qualified Fission.Config as Config
import           Fission.Internal.Constraint

import qualified Fission.File.Types      as File
import qualified Fission.IPFS.Types      as IPFS
import qualified Fission.IPFS.Client.Add as Add
import qualified Fission.IPFS.Client.Cat as Cat
import qualified Fission.IPFS.Client.Pin as Pin

type API
  = "api"
  :> "v0"
  :> V0API

type V0API = "cat" :> Cat.API
        :<|> "add" :> Add.API
        :<|> "pin" :> Pin.API

cat ::   Text        -> Streaming.ClientM File.Serialized
pin ::   Text        -> Streaming.ClientM Text
unpin :: Text -> Bool -> Streaming.ClientM Text
cat :<|> pin :<|> unpin = Streaming.client (Proxy :: Proxy API)

run :: MonadRIO cfg m
    => NFData a
    => Has IPFS.URL cfg
    => Streaming.ClientM a
    -> m (Either Streaming.ServantError a)
run query = do
  IPFS.URL url <- Config.get
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  liftIO . Streaming.runClientM query $ Streaming.mkClientEnv manager url

{-
It allows using this module's ClientM in a direct style. The NFData constraint however prevents using this function with genuine streaming response types (SourceT, Conduit, pipes Proxy or Machine). For those you have to use withClientM.
-}
