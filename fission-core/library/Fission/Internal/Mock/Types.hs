{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Mock.Types
  ( module Fission.Internal.Mock.Effect.Types
  , module Fission.Internal.Mock.Session.Types
  , Mock (..)
  ) where

import           Control.Monad.Catch                 as Catch
import           Control.Monad.Writer

import           Servant.Client.Core

import qualified Network.IPFS.Client.Pin             as Network.Pin
import qualified Network.IPFS.File.Types             as File
import           Network.IPFS.Remote.Class
import qualified Network.IPFS.Types                  as IPFS

import           Fission.Prelude

import qualified Fission.Internal.Mock.Effect        as Effect

-- Reexport

import           Fission.Internal.Mock.Effect.Types
import           Fission.Internal.Mock.Session.Types

{- | Fission's mock type

     Notes:
     * We will likely want @State@ in here at some stage
     * @RIO@ because lots of constraints want @MonadIO@
       * Avoid actual @IO@, or we're going to have to rework this 😉

-}
newtype Mock effs cfg a = Mock
  { unMock :: WriterT [OpenUnion effs] (RIO cfg) a }
  deriving
    newtype ( Functor
            , Applicative
            , Monad
            , MonadWriter [OpenUnion effs]
            , MonadReader cfg
            )

instance RunIO `IsMember` effs => MonadIO (Mock effs cfg) where
  liftIO action = do
    Effect.log RunIO
    Mock $ liftIO action

instance RunThrow `IsMember` effs => MonadThrow (Mock effs cfg) where
  throwM err = do
    Effect.log RunThrow
    Mock $ throwM err

instance (RunThrow `IsMember` effs, RunCatch `IsMember` effs) => MonadCatch (Mock effs cfg) where
  catch action handler = do
    Effect.log RunCatch
    Mock $ Catch.catch (unMock action) (unMock . handler)

instance (GetTime `IsMember` effs, HasField' "now" cfg UTCTime) => MonadTime (Mock effs cfg) where
  currentTime = do
    Effect.log GetTime
    asks $ getField @"now"

instance LogMsg `IsMember` effs => MonadLogger (Mock effs cfg) where
  monadLoggerLog _loc _src lvl msg =
    Effect.log . LogMsg lvl $ toLogStr msg

instance
  ( RunIO         `IsMember` effs
  , RunRemoteIPFS `IsMember` effs
  , HasField' "remoteIPFSAdd"   cfg (Either ClientError IPFS.CID)
  , HasField' "remoteIPFSCat"   cfg (Either ClientError File.Serialized)
  , HasField' "remoteIPFSPin"   cfg (Either ClientError Network.Pin.Response)
  , HasField' "remoteIPFSUnpin" cfg (Either ClientError Network.Pin.Response)
  )
  => MonadRemoteIPFS (Mock effs cfg) where
  runRemote _ = do
    Effect.log RemoteIPFSGeneric
    error "Directly called runRemote"

  ipfsAdd bs = do
    Effect.log $ RemoteIPFSAdd bs
    asks $ getField @"remoteIPFSAdd"

  ipfsCat cid = do
    Effect.log $ RemoteIPFSCat cid
    asks $ getField @"remoteIPFSCat"

  ipfsPin cid = do
    Effect.log $ RemoteIPFSPin cid
    asks $ getField @"remoteIPFSPin"

  ipfsUnpin cid flag = do
    Effect.log $ RemoteIPFSUnpin cid flag
    asks $ getField @"remoteIPFSUnpin"
