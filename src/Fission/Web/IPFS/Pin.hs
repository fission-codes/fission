module Fission.Web.IPFS.Pin
  ( API
  , server
  , pin
  , unpin
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Database.Selda
import Servant

import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS
import qualified Fission.Web.Error    as Web.Err
import           Fission.Web.Server
import           Fission.IPFS.CID.Types
import qualified Fission.User.CID     as UserCID

type API = PinAPI :<|> UnpinAPI

type PinAPI = Capture "cid" CID
           :> Put '[PlainText, OctetStream] NoContent

type UnpinAPI = Capture "cid" CID
             :> Delete '[PlainText, OctetStream] NoContent

server :: Has IPFS.BinPath  cfg
       => HasProcessContext cfg
       => MonadSelda   (RIO cfg)
       => HasLogFunc        cfg
       => RIOServer         cfg API
server = pin :<|> unpin

pin :: Has IPFS.BinPath  cfg
    => HasProcessContext cfg
    => MonadSelda   (RIO cfg)
    => HasLogFunc        cfg
    => RIOServer         cfg PinAPI
pin cid = do
  result <- Storage.IPFS.pin cid
  case result of
    Left err -> Web.Err.throw err
    Right ()  -> do
      void $ UserCID.createFresh undefined cid -- Record who FIXME undefined userID!!!!
      pure NoContent

unpin :: Has IPFS.BinPath  cfg
      => HasProcessContext cfg
      => HasLogFunc        cfg
      => RIOServer         cfg UnpinAPI
unpin = either Web.Err.throw (pure . const NoContent) <=< Storage.IPFS.unpin
