{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage.RIO () where

import Fission.Prelude

import RIO.Orphans ()

import qualified RIO.ByteString.Lazy as Lazy

import           Servant.Client
import           Network.HTTP.Client as HTTP

import           Network.AWS
import qualified Network.AWS.Auth as AWS

import           Network.IPFS
import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process
import qualified Network.IPFS.Peer as Peer

import qualified Fission.Config as Config

instance (Has AWS.AccessKey cfg, Has AWS.SecretKey cfg) => MonadAWS (RIO cfg) where
  liftAWS awsAction = do
    accessKey :: AWS.AccessKey <- Config.get
    secretKey :: AWS.SecretKey <- Config.get

    env <- newEnv <| FromKeys accessKey secretKey

    awsAction
      |> runAWS env
      |> runResourceT

instance
  ( HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => MonadLocalIPFS (RIO cfg) where
    runLocal opts arg = do
      IPFS.BinPath ipfs <- Config.get
      IPFS.Timeout secs <- Config.get
      let opts' = ("--timeout=" <> show secs <> "s") : opts

      runProc readProcess ipfs (byteStringInput arg) byteStringOutput opts' >>= \case
        (ExitSuccess, contents, _) ->
          return <| Right contents
        (ExitFailure _, _, stdErr)
          | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
              return . Left <| Process.Timeout secs
          | otherwise ->
            return . Left <| Process.UnknownErr stdErr

instance
  ( Has IPFS.URL     cfg
  , Has HTTP.Manager cfg
  , Has Peer         cfg
  , MonadLocalIPFS (RIO cfg)
  )
  => MonadRemoteIPFS (RIO cfg) where
    runRemote query = do
      peerID       <- Config.get
      IPFS.URL url <- Config.get
      manager      <- Config.get

      _ <- Peer.connectRetry peerID 2

      url
        |> mkClientEnv manager
        |> runClientM query
        |> liftIO

instance MonadTime (RIO cfg) where
  currentTime = liftIO getCurrentTime
