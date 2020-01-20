{-# LANGUAGE UndecidableInstances #-}

module Test.Types where

import           Control.Monad.Writer

import           Fission.Prelude

import           Fission.IPFS.Linked.Class
import qualified Network.IPFS.Types  as IPFS
-- import qualified Database.Persist.Sql as SQL
import           Fission.Web.Auth.Class
-- import           Fission.Models

import Data.Generics.Product

-- import RIO.HashMap
-- import RIO.State
import Servant

data MockSession log a = MockSession
  { effects :: [OpenUnion log]
  , result  :: a
  }

newtype FissionMock effs ctx a = FissonMock
  { unMock :: WriterT [OpenUnion effs] (RIO ctx) a }
  deriving
    newtype ( Functor
            , Applicative
            , Monad
            , MonadWriter [OpenUnion effs]
            , MonadReader ctx
            , MonadIO
            )

runMock :: MonadIO m => ctx -> FissionMock effs ctx a -> m (MockSession effs a)
runMock ctx action =
  action
    |> unMock
    |> runWriterT
    |> runRIO ctx
    |> fmap \(result, effects) -> MockSession {..}

logEff ::
  ( IsMember eff log
  , Applicative t
  , MonadWriter (t (OpenUnion log)) m
  )
  => eff
  -> m ()
logEff effect =
  effect
    |> openUnionLift
    |> pure
    |> tell

-- RunDB

data RunDB = RunDB
  deriving (Show, Eq)

instance IsMember RunDB effs => MonadDB (FissionMock effs cfg) (FissionMock effs cfg) where
  runDB mock = do
    logEff RunDB
    mock

-- IPFSLinkedPeer

newtype LinkedPeer = LinkedPeer IPFS.Peer
  deriving (Show, Eq)

instance IsMember LinkedPeer logs => MonadLinkedIPFS (FissionMock logs cfg) where
  getLinkedPeers = do
    let fakePeer = IPFS.Peer "/ip4/FAKE_PEER"
    logEff <| LinkedPeer fakePeer
    return <| pure fakePeer

-- Auth

data GetVerifier = GetVerifier
  deriving (Show, Eq)

instance
  ( IsMember GetVerifier effs
  , HasField' "authCheck" ctx (BasicAuthCheck usr)
  )
  => MonadAuth usr (FissionMock effs ctx) where
  verify = do
    logEff GetVerifier
    asks (getField @"authCheck")
