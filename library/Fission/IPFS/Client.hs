module Fission.IPFS.Client
  ( API
  , add
  , cat
  , pin
  , run
  , unpin
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import           SuperRecord

import qualified Network.HTTP.Client as HTTP
import           Servant
import           Servant.Client as Client

import           Fission.Internal.Constraint
import           Fission.Internal.Orphanage.ByteString.Lazy ()

import qualified Fission.File.Types      as File
import           Fission.IPFS.CID.Types

import qualified Fission.IPFS.Client.Add as Add
import qualified Fission.IPFS.Client.Cat as Cat
import qualified Fission.IPFS.Client.Pin as Pin

type API
  = "api"
  :> "v0"
  :> V0API

type V0API = "add" :> Add.API
        :<|> "cat" :> Cat.API
        :<|> "pin" :> Pin.API

add   :: Lazy.ByteString -> ClientM CID
cat   :: Text            -> ClientM File.Serialized
pin   :: Text            -> ClientM Pin.Response
unpin :: Text -> Bool     -> ClientM Pin.Response

add :<|> cat
    :<|> pin
    :<|> unpin = client (Proxy :: Proxy API)

run :: MonadRIO         (Rec cfg) m
    => Has "ipfsURL"     cfg Client.BaseUrl
    => Has "httpManager" cfg HTTP.Manager
    => ClientM a
    -> m (Either ClientError a)
run query = do
  url     <- asksR #ipfsURL
  manager <- asksR #httpManager
  liftIO . runClientM query $ mkClientEnv manager url
