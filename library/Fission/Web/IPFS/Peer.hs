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

type API = Get '[JSON, PlainText, OctetStream] [IPFS.Peer]

-- get :: Has IPFS.BinPath  cfg
--         => Has IPFS.Timeout  cfg
--         => HasProcessContext cfg
--         => HasLogFunc        cfg
--         => RIOServer         cfg API
-- get = case getExternalAddress of
--   -- This is a joke
--   -- why is it so hard to parse an either
--   -- god damn haskell
--   Right peers -> return peers
--   Left err ->  Web.Err.throw err
