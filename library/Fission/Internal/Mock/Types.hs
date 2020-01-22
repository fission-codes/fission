{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Mock.Types
  ( module Fission.Internal.Mock.Config.Types
  , module Fission.Internal.Mock.Effect.Types
  , module Fission.Internal.Mock.Session.Types
  , Mock (..)
  ) where

import           Control.Monad.Catch
import           Control.Monad.Trans.AWS

import           Network.IPFS.Remote.Class

import           Servant
import           Servant.Client

import qualified Fission.Web.Types as Web

import           Network.AWS

import           Control.Monad.Writer
import           Database.Esqueleto


import           Fission.Internal.Mock.Effect as Effect
import           Fission.Internal.Mock.Effect.Types
import           Fission.Internal.Mock.Config.Types

import           Fission.Prelude
import           Fission.IPFS.Linked.Class
import           Fission.Web.Auth.Class
import           Fission.Models
import Fission.IPFS.DNSLink.Class
import           Fission.Web.Server.Reflective.Class


import           Fission.AWS
import qualified Fission.Platform.Heroku.Auth.Types as Heroku

import           Fission.Internal.Mock.Session.Types
import           Network.IPFS.Local.Class

import           Fission.Internal.Mock.Config.Types as Mock

import           Fission.User                  as User
import           Fission.User.CID              as User.CID
import           Fission.Platform.Heroku.AddOn as Heroku.AddOn

{- | Fission's mock type

     Notes:
     * We will likely want @State@ in here at some stage
     * @RIO@ because lots of constraints want @MonadIO@
       * Avoid actual @IO@, or we're going to have to rework this 😉

-}
newtype Mock effs a = Mock
  { unMock :: WriterT [OpenUnion effs] (RIO Mock.Config) a }
  deriving
    newtype ( Functor
            , Applicative
            , Monad
            , MonadWriter [OpenUnion effs]
            , MonadReader Mock.Config
            , MonadIO
            , MonadThrow
            , MonadCatch
            )

instance IsMember RunDB effs => MonadDB (Mock effs) (Mock effs) where
  runDB transaction = do
    Effect.log RunDB
    transaction

instance IsMember GetLinkedPeers effs => MonadLinkedIPFS (Mock effs) where
  getLinkedPeers = do
    peerList <- asks linkedPeers
    Effect.log (GetLinkedPeers peerList)
    return peerList

instance IsMember GetVerifier effs => MonadAuth Text (Mock effs) where
  verify = do
    Effect.log GetVerifier
    isAuthed <- asks forceAuthed
    return <| BasicAuthCheck \_ ->
      return <| if isAuthed
                  then Authorized "YUP"
                  else Unauthorized

instance IsMember GetVerifier effs => MonadAuth (Entity User) (Mock effs) where
  verify = do
    Effect.log GetVerifier
    asks userVerifier

instance IsMember GetVerifier effs => MonadAuth Heroku.Auth (Mock effs) where
  verify = do
    Effect.log GetVerifier
    asks herokuVerifier

instance IsMember RunAWS effs => MonadAWS (Mock effs) where
  liftAWS awsAction = do
    Effect.log RunAWS
    env <- newEnv <| FromKeys "FAKE_ACCESS_KEY" "FAKE_SECRET_KEY"

    awsAction
      |> runAWST env
      |> runResourceT
      |> liftIO

instance IsMember CheckTime effs => MonadTime (Mock effs) where
  currentTime = do
    Effect.log CheckTime
    asks now

instance
  ( IsMember RunAWS        effs
  , IsMember UpdateRoute53 effs
  )
  => MonadRoute53 (Mock effs) where
  update r d t = do
    Effect.log UpdateRoute53
    runner <- asks updateRoute53
    return <| runner r d t

instance
  ( IsMember UpdateRoute53 effs
  , IsMember SetDNSLink    effs
  , IsMember RunAWS        effs
  )
  => MonadDNSLink (Mock effs) where
  set s c = do
    Effect.log SetDNSLink
    runner <- asks setDNSLink
    return <| runner s c

instance IsMember RunLocalIPFS effs => MonadLocalIPFS (Mock effs) where
  runLocal _ _ = do
    Effect.log RunLocalIPFS
    asks localIPFSCall

instance IsMember RunRemoteIPFS effs => MonadRemoteIPFS (Mock effs) where
  ipfsAdd bs = do
    Effect.log <| RemoteIPFSAdd bs
    asks remoteIPFSAdd

  ipfsCat cid = do
    Effect.log <| RemoteIPFSCat cid
    asks remoteIPFSCat

  ipfsPin cid = do
    Effect.log <| RemoteIPFSPin cid
    asks remoteIPFSPin

  ipfsUnpin cid flag = do
    Effect.log <| RemoteIPFSUnpin cid flag
    asks remoteIPFSUnpin

instance MonadReflectiveServer (Mock effs) where
  getHost = Web.Host <$> parseBaseUrl "example.com"

instance IsMember LogMsg effs => MonadLogger (Mock effs) where
  monadLoggerLog loc src lvl msg = do
    Effect.log <| LogMsg lvl <| toLogStr msg
    monadLoggerLog loc src lvl msg

instance IsMember DestroyHerokuAddOn effs => Heroku.AddOn.Destroyer (Mock effs) where
  destroyByUUID uuid = do
    Effect.log <| DestroyHerokuAddOn uuid
    pure ()

instance IsMember DestroyHerokuAddOn effs => Heroku.AddOn.Retriever (Mock effs) where
  getByUUID uuid = do
    Effect.log <| DestroyHerokuAddOn uuid
    pure Nothing

instance Heroku.AddOn.Creator (Mock effs) where
  create _ _ _ = do
    -- Effect.log <| DestroyHerokuAddOn uuid
    -- pure Nothing
    undefined

instance User.Retriever (Mock effs) where
  getByUsername _username = do
    -- Effect.log <| DestroyHerokuAddOn uuid
    pure Nothing

  getByHerokuAddOnId _ = do
    -- Effect.log <| DestroyHerokuAddOn uuid
    pure Nothing

instance IsMember DestroyHerokuAddOn effs => User.Creator (Mock effs) where
  create _ _ _ _ _ = do
    -- Effect.log <| DestroyHerokuAddOn uuid
    undefined

  createWithHeroku _ _ _ _ _ = do
    -- Effect.log <| DestroyHerokuAddOn uuid
    undefined

instance User.Modifier (Mock effs) where
  updatePassword _ _ _ = do
    undefined

instance User.Destroyer (Mock effs) where
  destroy _ = do
    undefined

instance User.CID.Retriever (Mock effs) where
  getByUserId _ = do
    undefined

  getByCids _ = do
    undefined

instance User.CID.Creator (Mock effs) where
  create _ _ _ = do
    undefined

  createMany _ _ _ = do
    undefined

instance User.CID.Destroyer (Mock effs) where
  destroy _ _ = do
    undefined

  destroyMany _ = do
    undefined
