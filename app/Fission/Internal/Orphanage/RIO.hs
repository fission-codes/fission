{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage.RIO () where

import Fission.Prelude

import Data.Pool
import RIO.Orphans ()

import Database.Selda.Backend.Internal
import Database.Selda.PostgreSQL

import qualified RIO.ByteString.Lazy as Lazy

import           Servant.Client
import           Network.HTTP.Client as HTTP

import qualified Fission.Config        as Config
import qualified Fission.Storage.Types as DB

import           Network.IPFS
import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process

instance Has (DB.Pool PG) cfg => MonadSelda (RIO cfg) where
  type Backend (RIO cfg) = PG

  withConnection action = do
    DB.Pool pool <- Config.get
    withResource pool action

instance 
  ( HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => MonadLocalIPFS (RIO cfg) where
    ipfsRun opts arg = do
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
  ( Has IPFS.URL cfg
  , Has HTTP.Manager cfg
  )
  => MonadRemoteIPFS (RIO cfg) where
    runRemote query = do
      IPFS.URL url            <- Config.get
      manager :: HTTP.Manager <- Config.get
      url
        |> mkClientEnv manager
        |> runClientM query
        |> liftIO
