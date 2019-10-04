module Fission.Web.IPFS.Peer where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Database.Selda

import qualified Network.HTTP.Client as HTTP
import           Servant

import Fission.IPFS.Peer
import Fission.Web.Server
import qualified Fission.IPFS.Types          as IPFS
import qualified Fission.Web.Error       as Web.Err

type API = Get '[OctetStream] [IPFS.Peer]

get :: Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => RIOServer         cfg API
get = getExternalAddress >>= \res -> do
  logInfo $ "GET ENDPOINT: " <> displayShow res
  case res of
    Right peers -> return peers
    Left err ->  Web.Err.throw err


  -- do
  -- result <- getExternalAddress
  -- -- This is a joke
  -- -- why is it so hard to parse an either
  -- -- god damn haskell
  -- case result of
  --   Right peers -> return peers
  --   Left err ->  Web.Err.throw err

  --   getExternalAddress >>= \result ->
  --     case result of
  --       Right peers -> return peers
  --       Left err ->  Web.Err.throw err

  -- ------

  -- getExternalAddress >>= \case
  --   Right peers -> return peers
  --   Left err ->  Web.Err.throw err


    -- a -> m b
    -- b -> m c
    -- c -> m d

    -- (a -> b) -> m a -> m b
    -- show -> [1,2,3] -> ["1", "2", "3'"]

    -- (a -> m b) -> m a -> m b
    -- \x -> [show x, show (x * 10)] -> [1,2,3] -> ["1", "10", "2", "20", "3", "30"]