module Fission.IPFS.Client
  ( API
  , add
  , cat
  , pin
  , run
  , unpin
  ) where

import           Flow
import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Has

import qualified Network.HTTP.Client as HTTP
import           Servant
import           Servant.Client

import qualified Fission.Config as Config
import           Fission.Internal.Constraint
import           Fission.Internal.Orphanage.ByteString.Lazy ()

import qualified Fission.File.Types      as File
import qualified Fission.IPFS.Types      as IPFS
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
unpin :: Text -> Bool    -> ClientM Pin.Response

add :<|> cat
    :<|> pin
    :<|> unpin = client $ Proxy @API

-- NOTE: May want to move these to streaming in the future
run :: MonadRIO         cfg m
    => Has IPFS.URL     cfg
    => Has HTTP.Manager cfg
    => ClientM a
    -> m (Either ClientError a)
run query = do
  IPFS.URL url            <- Config.get
  manager :: HTTP.Manager <- Config.get

  url
    |> mkClientEnv manager
    |> runClientM query
    |> liftIO

  -- liftIO <| runClientM query <| mkClientEnv manager url
