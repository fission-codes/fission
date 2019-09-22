{-# LANGUAGE MonoLocalBinds #-}

-- | Top level web application and API
module Fission.Web
  ( API
  , app
  , server
  ) where

import           RIO
import           RIO.Process (HasProcessContext)
import qualified RIO.Text as Text

import           SuperRecord
import           Database.Selda
import           Data.Swagger as Swagger
import qualified Network.HTTP.Client as HTTP
import           Servant

import qualified Fission.Config     as Config
import           Fission.User
import           Fission.Web.Server
import qualified Fission.IPFS.Types as IPFS
import           Fission.File.Types ()
import           Fission.Internal.Orphanage.PlainText ()

import qualified Fission.Web.Auth        as Auth
import qualified Fission.Web.Auth.Verify as Auth
import qualified Fission.Web.IPFS        as IPFS
import qualified Fission.Web.Ping        as Ping
import qualified Fission.Web.Routes      as Web
import qualified Fission.Web.Swagger     as Web.Swagger
import qualified Fission.Web.Types       as Web

import qualified Fission.Platform.Heroku.Types as Heroku
import qualified Fission.Web.Heroku            as Heroku

-- | Top level web API type. Handled by 'server'.
type API = Web.Swagger.API :<|> Web.API

-- | The actual web server for 'API'
app :: HasOf [ "ipfsPath"    := IPFS.BinPath
            , "ipfsTimeout" := IPFS.Timeout
            , "ipfsURL" := IPFS.URL
            , "httpManager" := HTTP.Manager
            , "host" := Web.Host
            , "herokuID" := Heroku.ID
            , "herokuPassword" := Heroku.Password
            ] cfg
    => HasProcessContext   (Rec cfg)
    => HasLogFunc          (Rec cfg)
    => MonadSelda     (RIO (Rec cfg))
    =>     cfg
    -> RIO cfg Application
app cfg = do
  auth             <- mkAuth
  Web.Host appHost <- Config.get
  return . serveWithContext api auth
         . Auth.server api cfg
         $ server (Swagger.Host (Text.unpack appHost) Nothing)
  where
    api :: Proxy API
    api = Proxy

-- | Construct an authorization context
mkAuth :: Has "herokuID" cfg Heroku.ID
       => Has "herokuPassword" cfg Heroku.Password
       => HasLogFunc          (Rec cfg)
       => MonadSelda     (RIO (Rec cfg))
       => RIO cfg (Context '[BasicAuthCheck User, BasicAuthCheck ByteString])
mkAuth = do
  Heroku.ID       hkuID   <- Config.get
  Heroku.Password hkuPass <- Config.get

  let hku = Auth.basic hkuID hkuPass
  usr <- Auth.user

  return $ usr
        :. hku
        :. EmptyContext

-- | Web handlers for the 'API'
server :: Has "ipfsPath" cfg IPFS.BinPath
       => Has "ipfsTimeout" cfg IPFS.Timeout
       => Has "httpManager" cfg HTTP.Manager
       => Has "ipfsURL" cfg IPFS.URL
       => Has "host" cfg Web.Host
       => HasProcessContext (Rec cfg)
       => HasLogFunc        (Rec cfg)
       => MonadSelda   (RIO (Rec cfg))
       => Swagger.Host
       -> RIOServer         (Rec cfg) API
server host' = Web.Swagger.server host'
          :<|> IPFS.server
          :<|> const Heroku.server
          :<|> const Auth.verify
          :<|> pure Ping.pong
