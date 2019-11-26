module Fission.Web.IPFS.Upload
  ( API
  , add
  ) where

import           Database.Selda

import           Servant

import           Fission.Prelude

import qualified Fission.Web.IPFS.Upload.Multipart as Multipart
import qualified Fission.Web.IPFS.Upload.Simple    as Simple
import           Fission.Web.Server
import           Fission.User

import           Network.IPFS

type API = Simple.API :<|> Multipart.API

add ::
  ( MonadSelda     (RIO cfg)
  , MonadLocalIPFS (RIO cfg)
  , HasLogFunc          cfg
  )
  => User
  -> RIOServer cfg API
add usr = Simple.add    usr
     :<|> Multipart.add usr
