{-# LANGUAGE UndecidableInstances #-}

module Fission.Test.CLI.Handler.App.Publish (spec) where

import           Control.Monad.Writer
import qualified RIO.ByteString.Lazy                 as Lazy
import qualified RIO.Text                            as Text

import qualified Crypto.PubKey.Ed25519               as Ed25519
import qualified Data.Yaml                           as YAML

import           Network.HTTP.Client
import qualified Network.IPFS.Process.Error          as IPFS.Process
import qualified Network.Wai.Handler.Warp            as Warp
import           Servant.Client

import           Fission.CLI.Handler.App.Publish
import           Fission.Error
import           Fission.Internal.Mock               as Effect
import           Fission.Internal.Mock               as Mock

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT          as JWT
import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client.Auth.Class
import           Fission.Web.Client.Class

import           Fission.Test.CLI.Prelude

-----------
-- Setup --
-----------

data Config = Config
  { ed25519SK :: Ed25519.SecretKey
  , webPort   :: Int
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
    manager   <- MockPublish . Mock . liftIO $ newManager defaultManagerSettings
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

-- FIXME
withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action =
  Warp.testWithApplication (pure undefined) action
  -- Warp.testWithApplication (pure userApp) action

spec =
  around withUserApp do
    describe "Fission.CLI.Handler.App.Publish" do
      describe "runRequest" do
        it "retries with a different UCAN" \port -> do
          pending
