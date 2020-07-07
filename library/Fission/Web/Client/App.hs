module Fission.Web.Client.App
  ( Create
  , Update
  , mkUpdateReq
  ) where

import Fission.Prelude

import           Servant
import           Servant.Client


import           Fission.Web.Client     as Client
import           Fission.Web.Routes     (AppPrefix)
import qualified Fission.Web.App.Create as App.Create
import qualified Fission.Web.App.Update as App.Update
import qualified Fission.Web.Auth.Types as Auth
import           Fission.Web.Auth.Token

import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Network.IPFS.CID.Types

import           Fission.URL
import           Fission.Authorization.ServerDID


type Create
  = AppPrefix
  :> Auth.HigherOrder
  :> App.Create.API

type Update
  = AppPrefix
  :> Auth.HigherOrder
  :> App.Update.API

mkUpdateReq ::
  ( MonadIO      m
  , MonadTime    m
  , MonadLogger  m
  , ServerDID    m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => URL
  -> CID
  -> Bool
  -> m (ClientM NoContent)
mkUpdateReq url cid copyFiles =
  authClient (Proxy @Update)
    `withPayload` url
    `withPayload` cid
    `withPayload` Just copyFiles
