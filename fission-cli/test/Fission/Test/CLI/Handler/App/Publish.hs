{-# LANGUAGE UndecidableInstances #-}

module Fission.Test.CLI.Handler.App.Publish (spec) where

import           Control.Monad.Writer
import qualified RIO.ByteString.Lazy                 as Lazy
import qualified RIO.Text                            as Text

import qualified Crypto.PubKey.Ed25519               as Ed25519
import qualified Data.Yaml                           as YAML

import qualified Network.HTTP.Client                 as HTTP
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Process.Error          as IPFS.Process
import qualified Network.Wai.Handler.Warp            as Warp

import           Servant.API                         (AuthProtect, parseHeader,
                                                      (:>))
import           Servant.API.Generic
import           Servant.Client
import           Servant.Server
import           Servant.Server.Experimental.Auth
import           Servant.Server.Generic

import           Fission.Error
import           Fission.Internal.Mock               as Mock
import           Fission.Internal.Mock.Effect        as Effect
import           Fission.URL.Types
import           Fission.Web.Auth.Token.JWT.Types

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT          as JWT
import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client.Auth.Class
import           Fission.Web.Client.Class

import qualified Fission.Web.API.App.Types           as App
import           Fission.Web.API.Types

import           Fission.CLI.Handler.App.Publish

import           Fission.Test.CLI.Prelude



--------
import qualified Network.HTTP.Client                 as HTTP
import qualified Servant.Client.Internal.HttpClient  as I

-----------
-- Setup --
-----------

data Config = Config
  { ed25519SK :: Ed25519.SecretKey
  , webPort   :: Warp.Port
  }

newtype MockPublish effs a = MockPublish
  { getMock :: Mock effs Config a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadWriter [OpenUnion effs]
                   , MonadReader Config
                   )

instance RunThrow `IsMember` effs => MonadThrow (MockPublish effs) where
  throwM e = MockPublish $ throwM e

instance GetTime `IsMember` effs => MonadTime (MockPublish effs) where
  currentTime = do
    Effect.log GetTime
    MockPublish . Mock $ liftIO getCurrentTime

instance
  ( WebRequest `IsMember` effs
  , RunThrow   `IsMember` effs
  )
  => MonadWebClient (MockPublish effs) where
  sendRequest clientM = do
    port'     <- asks webPort
    localhost <- parseBaseUrl "http://localhost"
    manager   <- MockPublish . Mock . liftIO $ HTTP.newManager HTTP.defaultManagerSettings
    let clientEnv = mkClientEnv manager (localhost { baseUrlPort = port' })

    Effect.log WebRequest
    MockPublish . Mock . liftIO $ runClientM clientM clientEnv

instance GetTime `IsMember` effs => MonadWebAuth (MockPublish effs) Token where
  getAuth = do
    now <- currentTime
    sk  <- getAuth
    did <- undefined -- FIXME

    return $ Bearer Bearer.Token
      { jwt        = JWT.delegateAppendAll did sk RootCredential now
      , rawContent = RawContent "FAKE"
      }

instance MonadWebAuth (MockPublish effs) Ed25519.SecretKey where
  getAuth = asks ed25519SK

instance MonadRaise (MockPublish effs) where
  type Errors (MockPublish effs)
    = '[ SomeException
       , YAML.ParseException
       , ClientError
       , IPFS.Process.Error
       , NotFound FilePath
       ]

  raise _ = return undefined

instance MonadRescue (MockPublish effs) where
  attempt action = Right <$> action

instance MonadCleanup (MockPublish effs) where
  cleanup res _ _ action = action =<< res

-----------
-- Tests --
-----------

type instance AuthServerData (AuthProtect "higher-order") = Token

-- FIXME
withWebApp :: (Warp.Port -> IO ()) -> IO ()
withWebApp action = do
  isSuccessVar <- newEmptyMVar

  let
    appUpdateTestHandler :: URL -> CID -> Maybe Bool -> Token -> IO ()
    appUpdateTestHandler _ _ _ _ = do
      tryReadMVar isSuccessVar >>= \case
        Nothing ->  do
          isSuccessVar `swapMVar` ()
          throwM err502

        Just () ->
          return ()

    authCtx :: Context '[AuthHandler HTTP.Request Token]
    authCtx = authHandler' :. EmptyContext

    authHandler' :: AuthHandler HTTP.Request Token
    authHandler' =
      mkAuthHandler \req ->
        case lookup "Authorization" (HTTP.requestHeaders req) of
          Nothing ->
            error "No auth token"

          Just auth ->
            case parseHeader auth of
              Left errMsg -> error "Can't parse"
              Right token -> return token

    appHandler =
      App.RoutesV2
        { index   = undefined
        , create  = undefined
        , update  = undefined -- appUpdateTestHandler
        , destroy = undefined
        }

    routeProxy :: Proxy ("v2" :> "api" :> "app" :> ToServantApi App.RoutesV2)
    routeProxy = Proxy

    testWebApp :: Application
    testWebApp = serveWithContext routeProxy authCtx (genericServer appHandler)

  Warp.testWithApplication (pure testWebApp) action

spec =
  around withWebApp do
    describe "Fission.CLI.Handler.App.Publish" do
      describe "runRequest" do
        it "retries with a different UCAN" \port -> do
          pending
