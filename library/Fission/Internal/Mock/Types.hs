{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Mock.Types
  ( module Fission.Internal.Mock.Config.Types
  , module Fission.Internal.Mock.Effect.Types
  , module Fission.Internal.Mock.Session.Types
  , Mock (..)
  ) where

import           Control.Monad.Catch
import           Control.Monad.Trans.AWS
import           Control.Monad.Writer

import           Database.Esqueleto as Database

import           Network.IPFS.Local.Class
import           Network.IPFS.Remote.Class
import qualified Network.IPFS.Types as IPFS
import           Network.AWS

import           Servant
import           Servant.Client

import           Fission.Internal.Fixture            as Fixture
import           Fission.Internal.Mock.Effect        as Effect
import           Fission.Internal.Mock.Config.Types  as Mock
import           Fission.Internal.Mock.Session.Types

import           Fission.Internal.Mock.Effect.Types -- for reexport
import           Fission.Internal.Mock.Config.Types -- for reexport

import           Fission.Prelude
import           Fission.IPFS.Linked.Class
import           Fission.Web.Auth.Class
import           Fission.Models
import           Fission.IPFS.DNSLink.Class
import           Fission.Web.Server.Reflective.Class
import qualified Fission.Web.Types as Web

import           Fission.AWS
import qualified Fission.Platform.Heroku.Auth.Types as Heroku

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

instance MonadLinkedIPFS (Mock effs) where
  getLinkedPeers = do
    peerList <- asks linkedPeers
    return peerList

instance MonadAuth Text (Mock effs) where
  verify = do
    isAuthed <- asks forceAuthed
    return <| BasicAuthCheck \_ ->
      return <| if isAuthed
                  then Authorized "YUP"
                  else Unauthorized

instance MonadAuth (Entity User) (Mock effs) where
  verify = asks userVerifier

instance MonadAuth Heroku.Auth (Mock effs) where
  verify = asks herokuVerifier

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
  runRemote = undefined
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
    return Nothing

instance IsMember CreateHerokuAddOn effs => Heroku.AddOn.Creator (Mock effs) where
  create uuid _ _ = do
    Effect.log <| CreateHerokuAddOn uuid
    return . Right <| Database.toSqlKey 0

instance IsMember RetrieveUser effs => User.Retriever (Mock effs) where
  getByUsername username = do
    Effect.log <| GetUserByUsername username
    return . Just <| Fixture.entity Fixture.user

  getByHerokuAddOnId id = do
    Effect.log <| GetUserByHerokuAddOnId id
    pure <| Just <| Fixture.entity Fixture.user

instance
  ( IsMember CreateHerokuAddOn effs
  , IsMember CreateUser        effs
  )
  => User.Creator (Mock effs) where
  create _ _ _ _ _ = do
    Effect.log CreateUser
    return . Right <| Database.toSqlKey 0

  createWithHeroku uuid _ _ _ _ = do
    Effect.log CreateUser
    Effect.log <| CreateHerokuAddOn uuid
    return . Right <| Database.toSqlKey 0

instance IsMember ModifyUser effs => User.Modifier (Mock effs) where
  updatePassword uID password _ = do
    Effect.log <| ModifyUser uID
    return <| Right password

instance IsMember DestroyUser effs => User.Destroyer (Mock effs) where
  destroy uid = Effect.log <| DestroyUser uid

instance IsMember RetrieveUserCID effs => User.CID.Retriever (Mock effs) where
  getByUserId uid = do
    Effect.log <| GetUserCIDByUserId uid
    let
      userId = Database.toSqlKey 0
      cid    = IPFS.CID "Qm12345"
    return . pure . Fixture.entity <| UserCid userId cid Fixture.agesAgo Fixture.agesAgo
    -- UserCID fixture goes here

  getByCids cids =
    cids
      |> foldr folder (0, [])
      |> snd
      |> sequence
    where
      folder cid (counter, acc) =
        (counter + 1, action cid counter : acc)

      action :: IPFS.CID -> Int64 -> Mock effs (Entity UserCid)
      action cid rawUserId = do
        let userId = Database.toSqlKey rawUserId
        Effect.log <| GetUserCIDByCID cid
        return . Fixture.entity <| UserCid userId cid Fixture.agesAgo Fixture.agesAgo

instance IsMember CreateUserCID effs => User.CID.Creator (Mock effs) where
  create uid cid _ = do
    Effect.log <| CreateUserCID uid cid
    return . Just <| Database.toSqlKey 0

  createMany uid cids _ = do
    forM_ cids \cid ->
      Effect.log <| CreateUserCID uid cid

    return cids

instance IsMember DestroyUserCID effs => User.CID.Destroyer (Mock effs) where
  destroy uid cid =
    Effect.log <| DestroyUserCID uid cid

  destroyMany cidIds =
    forM_ cidIds \id ->
      Effect.log <| DestroyUserCIDById id
