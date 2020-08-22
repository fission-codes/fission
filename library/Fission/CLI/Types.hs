{-# LANGUAGE UndecidableInstances #-}

module Fission.CLI.Types
  ( FissionCLI (..)
  , runFissionCLI
  ) where

import qualified Crypto.PubKey.Ed25519               as Ed25519
import qualified Data.ByteString.Char8               as BS8

import           Control.Monad.Catch                 as Catch

import qualified RIO.ByteString.Lazy                 as Lazy
import qualified RIO.Text                            as Text


import qualified Network.DNS                         as DNS
import           Network.HTTP.Client                 as HTTP
import           Network.IPFS
import           Network.IPFS.Process
import qualified Network.IPFS.Process.Error          as Process
import           Network.IPFS.Types                  as IPFS

import           Servant.Client

import           Fission.Prelude                     hiding (mask,
                                                      uninterruptibleMask)

import           Fission.Authorization.ServerDID
import           Fission.Error.NotFound.Types

import           Fission.Key                         as Key
import           Fission.User.DID.Types

import           Fission.CLI.Base.Types              as Base
import           Fission.CLI.Connected.Types         as Connected

import           Fission.Web.Auth.Token
import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT          as JWT

import           Fission.Web.Client
import qualified Fission.Web.Client.JWT              as JWT

import qualified Fission.CLI.Display.Error           as CLI.Error
import qualified Fission.CLI.Display.Loader          as CLI

import           Fission.CLI.Environment.Class

newtype FissionCLI errs cfg a = FissionCLI
  { unFissionCLI :: RescueT errs (RIO cfg) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader cfg
                   , MonadThrow
                   , MonadCatch
                   , MonadRaise
                   , MonadRescue
                   )

runFissionCLI :: forall errs m cfg a .
  MonadIO m
  => cfg
  -> FissionCLI errs cfg a
  -> m (Either (OpenUnion errs) a)
runFissionCLI cfg = runRIO cfg . runRescueT . unFissionCLI

instance HasLogFunc cfg => MonadLogger (FissionCLI errs cfg) where
  monadLoggerLog loc src lvl msg =
    FissionCLI (RescueT (Right <$> monadLoggerLog loc src lvl msg))

instance
  ( Contains errs errs
  , IsMember SomeException errs
  , HasField' "httpManager" cfg HTTP.Manager
  , HasField' "fissionURL"  cfg BaseUrl
  )
  => MonadWebClient (FissionCLI errs cfg) where
  sendRequest req =
    CLI.withLoader 50_000 do
      manager <- asks $ getField @"httpManager"
      baseUrl <- asks $ getField @"fissionURL"

      liftIO . runClientM req $ mkClientEnv manager baseUrl

instance MonadTime (FissionCLI errs cfg) where
  currentTime = liftIO getCurrentTime

instance ServerDID (FissionCLI errs Connected.Config) where
  getServerDID = asks serverDID

instance ServerDID (FissionCLI errs Base.Config) where
  getServerDID =
    asks cachedServerDID >>= \case
      Just did ->
        return did

      Nothing -> do
        rs      <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf
        baseURL <- asks Base.fissionURL
        let url = BS8.pack $ "_did." <> baseUrlHost baseURL

        logDebug $ "No cached server DID. Fetching from " <> decodeUtf8Lenient url

        liftIO (DNS.withResolver rs \resolver -> DNS.lookupTXT resolver url) >>= \case
          Left errs -> do
            CLI.Error.put errs "Unable to find Fission's ID online"
            throwM errs

          Right [] -> do
            CLI.Error.put (NotFound @DID) $
              "No TXT record at " <> decodeUtf8Lenient url

            throwM $ NotFound @DID

          Right (didTxt : _) ->
            case eitherDecodeStrict ("\"" <> didTxt <> "\"") of
              Left errs -> do
                CLI.Error.put errs "Unable to find Fission's ID online"
                throwM $ NotFound @DID

              Right did -> do
                -- FIXME write the value to disk / root dir
                return did

instance
  ( IsMember Key.Error errs
  , IsMember (NotFound Ed25519.SecretKey) errs
  , ServerDID (FissionCLI errs cfg)
  , HasField' "fissionURL" cfg BaseUrl
  )
  =>  MonadWebAuth (FissionCLI errs cfg) Token where
  getAuth = do
    now       <- currentTime
    sk        <- getAuth
    serverDID <- getServerDID

    let
      jwt =
        JWT.ucan now serverDID sk RootCredential

      rawContent =
        jwt
          |> encode
          |> Lazy.toStrict
          |> decodeUtf8Lenient
          |> Text.dropPrefix "\""
          |> Text.dropSuffix "\""
          |> JWT.contentOf

    return $ Bearer Bearer.Token {..}

instance
  ( Key.Error                  `IsMember` errs
  , NotFound Ed25519.SecretKey `IsMember` errs
  )
  => MonadWebAuth (FissionCLI errs cfg) Ed25519.SecretKey where
  getAuth =
    Key.exists >>= \case
      True  -> readEd
      False -> raise $ NotFound @Ed25519.SecretKey

instance MonadMask (FissionCLI errs cfg) where
  mask action = do
    cfg <- ask
    FissionCLI . RescueT . liftIO $ mask \u ->
      fissionToIO cfg (action $ q cfg u)

  uninterruptibleMask action = do
    cfg <- ask
    FissionCLI . RescueT . liftIO $ uninterruptibleMask \u ->
      fissionToIO cfg (action $ q cfg u)

  generalBracket acquire release use = do
    cfg <- ask

    FissionCLI . RescueT $ liftIO do
      (rb, rme) <- generalBracket
        (fissionToIO cfg acquire)
        (innerRelease cfg)
        (innerUse     cfg)

      return do
        me <- rme
        b  <- rb
        return (b, me)

    where
      innerRelease _ (Left err) _ =
        return $ Left err

      innerRelease cfg (Right resource) exitCase =
        let
          runRelease = fissionToIO cfg . release resource
        in
          case exitCase of
            ExitCaseSuccess (Right val) -> runRelease $ ExitCaseSuccess val
            ExitCaseException err       -> runRelease $ ExitCaseException err
            _                           -> runRelease ExitCaseAbort

      innerUse  _   (Left  err) = return $ Left err
      innerUse cfg (Right val)  = fissionToIO cfg $ use val

-- Same as runBase from module above
fissionToIO :: cfg -> FissionCLI errs cfg a -> IO (Either (OpenUnion errs) a)
fissionToIO cfg action = runRIO cfg . runRescueT $ unFissionCLI action

q :: cfg
  -> (IO (Either (OpenUnion errs) a) -> IO (Either (OpenUnion errs) a))
  -> FissionCLI errs cfg a
  -> FissionCLI errs cfg a
q cfg u = FissionCLI . RescueT . liftIO . u . fissionToIO cfg

instance (Contains errs errs, IsMember SomeException errs)
  => MonadCleanup (FissionCLI errs cfg) where
  cleanup acquire onErr onOk action =
    mask $ \restore -> do
      resource <- acquire

      attempt (restore $ action resource) >>= \case
        Left errs -> do
          _ <- Catch.uninterruptibleMask_ $
                 fmap (\_ -> ()) (onErr resource errs)
                   `Catch.catch` \(_ :: SomeException) -> return ()

          raise errs

        Right output -> do
          _ <- onOk resource
          return output

instance
  ( HasField' "ipfsPath"    cfg IPFS.BinPath
  , HasField' "ipfsTimeout" cfg IPFS.Timeout
  , HasProcessContext       cfg
  , HasLogFunc              cfg
  )
  => MonadLocalIPFS (FissionCLI errs cfg) where
  runLocal opts arg = do
    IPFS.BinPath ipfs <- asks (getField @"ipfsPath")
    IPFS.Timeout secs <- asks (getField @"ipfsTimeout")

    let opts' = ("--timeout=" <> show secs <> "s") : opts

    runProc readProcess ipfs (byteStringInput arg) byteStringOutput opts' <&> \case
      (ExitSuccess, contents, _) ->
        Right contents

      (ExitFailure _, _, stdErrs)
        | Lazy.isSuffixOf "context deadline exceeded" stdErrs ->
            Left $ Process.Timeout secs

        | otherwise ->
            Left $ Process.UnknownErr stdErrs

instance
  HasField' "ignoredFiles" cfg IPFS.Ignored
  => MonadEnvironment (FissionCLI errs cfg) where
    getIgnoredFiles = asks $ getField @"ignoredFiles"
