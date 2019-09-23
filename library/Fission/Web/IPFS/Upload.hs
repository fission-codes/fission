module Fission.Web.IPFS.Upload
  ( API
  , add
  ) where

import RIO
import RIO.Process (HasProcessContext)

import SuperRecord
import Database.Selda

import qualified Network.HTTP.Client as HTTP
import           Servant
import qualified Servant.Client      as Client

import qualified Fission.Web.IPFS.Upload.Multipart as Multipart
import qualified Fission.Web.IPFS.Upload.Simple    as Simple
import qualified Fission.IPFS.Types   as IPFS
import           Fission.Web.Server
import           Fission.User

type API = Simple.API :<|> Multipart.API

add :: Has "ipfsPath"    cfg IPFS.BinPath
    => Has "ipfsTimeout" cfg Natural
    => Has "httpManager" cfg HTTP.Manager
    => Has "ipfsURL"     cfg Client.BaseUrl
    => HasProcessContext (Rec cfg)
    => HasLogFunc        (Rec cfg)
    => MonadSelda   (RIO (Rec cfg))
    => User
    -> RIOServer         (Rec cfg) API
add usr = Simple.add usr :<|> Multipart.add usr
