{-# LANGUAGE UndecidableInstances #-}

module Fission.Test.CLI.Handler.App.Publish (spec) where

import           Control.Monad.Writer
import qualified RIO.ByteString.Lazy                               as Lazy
import qualified RIO.Text                                          as Text

import qualified Crypto.PubKey.Ed25519                             as Ed25519
import qualified Data.Yaml                                         as YAML

import qualified Network.HTTP.Client                               as HTTP
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Process.Error                        as IPFS.Process
import qualified Network.Wai.Handler.Warp                          as Warp

import           Servant.API                                       (AuthProtect,
                                                                    BasicAuthData (..),
                                                                    parseHeader,
                                                                    (:>))
import           Servant.API.Generic
import           Servant.Client
import           Servant.Server
import           Servant.Server.Experimental.Auth
import           Servant.Server.Generic

import           Fission.Error
import           Fission.Internal.Mock                             as Mock
import           Fission.Internal.Mock.Effect                      as Effect
import           Fission.URL.Types
import           Fission.Web.Auth.Token.JWT.Types

import qualified Fission.Web.Auth.Token.Bearer.Types               as Bearer
import           Fission.Web.Auth.Token.JWT                        as JWT
import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client.Auth.Class
import           Fission.Web.Client.Class

import qualified Fission.Web.API.App.Types                         as App
import           Fission.Web.API.Types

import qualified Fission.CLI.Handler.App.Publish                   as App.Handler

import           Fission.Test.CLI.Prelude



--------
import           Fission.Authorization.ServerDID.Class
import           Fission.CLI.Environment.Class
import           Fission.CLI.IPFS.Daemon.Class
import           Fission.CLI.Parser.Open.Types
import           Fission.CLI.Parser.Watch.Types
import           Fission.CLI.Remote.Class
import qualified Fission.CLI.WebNative.FileSystem.Auth.Store.Class as WNFS
import qualified Fission.CLI.WebNative.Mutation.Auth.Store.Class   as Mutation
import           Fission.Key.Asymmetric.Public.Types
import           Fission.User.DID.Types
import           Fission.Web.API.Auth.Types                        as Auth
import qualified Fission.Web.API.Heroku.Auth.Types                 as Heroku
import qualified Network.HTTP.Client                               as HTTP
import           Network.IPFS.Local.Class
import qualified Network.Wai.Internal                              as Wai
import           RIO.FilePath
import qualified Servant.Client.Internal.HttpClient                as I

-----------
-- Setup --
-----------

data Config = Config
  { ed25519SK :: Ed25519.SecretKey
  , webPort   :: Warp.Port
  }

newtype MockPublish effs a = MockPublish
  { getMock :: RescueT  (Mock effs Config a) }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadWriter [OpenUnion effs]
                   , MonadReader Config
                   )

instance RunIO `IsMember` effs => MonadIO (MockPublish effs) where
  liftIO io = MockPublish $ liftIO io

instance LogMsg `IsMember` effs => MonadLogger (MockPublish effs) where
  monadLoggerLog _loc _src lvl msg = Effect.log . LogMsg lvl $ toLogStr msg

instance RunLocalIPFS `IsMember` effs => MonadLocalIPFS (MockPublish effs) where
  runLocal _ _ = do
    Effect.log RunLocalIPFS
    return $ Right "IPFS CLI content"

instance RunIPFSDaemon `IsMember` effs => MonadIPFSDaemon (MockPublish effs) where
  runDaemon = do
    Effect.log RunIPFSDaemon
    MockPublish . Mock . startProcess $ fromString "echo 123"

  checkRunning = do
    Effect.log RunIPFSDaemon
    return True

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
    did <- error "DID"

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

  raise _ = return ()

instance MonadRescue (MockPublish effs) where
  attempt action = Right <$> action

instance MonadCleanup (MockPublish effs) where
  cleanup res _ _ action = action =<< res

instance WNFS.MonadStore (MockPublish effs) where
  set _ _ _ = return () -- FIXME
  getAllMatching _ _ = return mempty

instance Mutation.MonadStore (MockPublish effs) where
  insert _ = error "insert"
  getAll   = pure mempty

instance MonadEnvironment (MockPublish effs) where
  getGlobalPath = MockPublish getGlobalPath

instance MonadRemote (MockPublish effs) where
  getRemote = MockPublish getRemote

instance ServerDID (MockPublish effs) where
  getServerDID = do
    sk <- MockPublish . Mock $ liftIO Ed25519.generateSecretKey
    pure $ DID Key (Ed25519PublicKey $ Ed25519.toPublic sk)

-----------
-- Tests --
-----------

type instance AuthServerData (AuthProtect "higher-order") = Token
type Checks = '[AuthHandler Wai.Request Token, BasicAuthCheck Heroku.Auth]

withWebApp :: (Warp.Port -> IO ()) -> IO ()
withWebApp action = do
  isSuccessVar <- newEmptyMVar

  let
    appUpdateTestHandler :: URL -> CID -> Maybe Bool -> Token -> Handler ()
    appUpdateTestHandler _ _ _ _ = do
      tryReadMVar isSuccessVar >>= \case
        Nothing ->  do
          isSuccessVar `swapMVar` ()
          throwM err502

        Just () ->
          return ()

    authCtx :: Context Checks
    authCtx = authHandler :. herokuAuth :. EmptyContext

    herokuAuth :: BasicAuthCheck Heroku.Auth
    herokuAuth = BasicAuthCheck \(BasicAuthData _ _) -> pure . Authorized $ Heroku.Auth "FAKE"

    authHandler :: AuthHandler Wai.Request Token
    authHandler =
      mkAuthHandler \req ->
        case lookup "Authorization" (Wai.requestHeaders req) of
          Nothing ->
            error "No auth token"

          Just auth ->
            case parseHeader auth of
              Left errMsg -> error "Can't parse"
              Right token -> return token

    routeProxy :: Proxy ("v2" :> "api" :> "app" :> App.Update)
    routeProxy = Proxy

    testWebApp :: Application
    testWebApp = serveWithContext routeProxy authCtx appUpdateTestHandler

  Warp.testWithApplication (pure testWebApp) action

spec :: Spec
spec =
  around withWebApp do
    describe "Fission.CLI.Handler.App.Publish" do
      describe "runRequest" do
        it "retries with a different UCAN" \(webPort :: Warp.Port) -> do
          ed25519SK <- Ed25519.generateSecretKey
          let path = "tmp" </> "runRequest" </> "fission.yaml"

          Mock.Session {effectLog, result} <- runMockPublish Config {ed25519SK, webPort} (runPublish path)
          1 `shouldBe` 1

runPublish :: FilePath -> MockPublish '[GetTime, RunThrow, WebRequest, RunIO, LogMsg, RunLocalIPFS, RunIPFSDaemon] ()
runPublish appPath =
  App.Handler.publish (OpenFlag False) (WatchFlag False) (error "Not in watch mode") url appPath True True
  where
    url = URL (DomainName "example.com") Nothing

runMockPublish :: Config -> MockPublish effs a -> IO (Mock.Session effs a)
runMockPublish cfg (MockPublish mockAction) = runMock cfg mockAction
