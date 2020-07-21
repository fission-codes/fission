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

import           Servant.Client
import           Servant.Server

import           Fission.Internal.Fixture            as Fixture
import           Fission.Internal.Mock.Effect        as Effect
import           Fission.Internal.Mock.Config.Types  as Mock
import           Fission.Internal.Mock.Session.Types
import qualified Fission.Internal.Fixture.Key.Ed25519 as Ed25519

import           Fission.Prelude
 
import           Fission.URL

import           Fission.Authorization.Types
import           Fission.Authorization.Potency.Types

import           Fission.IPFS.Linked.Class
import           Fission.IPFS.DNSLink.Class

import           Fission.Models
import           Fission.User.DID.Types

import           Fission.Web.Auth.Class
import           Fission.Web.Client.Auth.Class

import           Fission.Web.Server.Reflective.Class
import qualified Fission.Web.Types as Web

import           Fission.Web.Auth.Token.Basic.Class

import           Fission.Web.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import           Fission.AWS
import qualified Fission.Platform.Heroku.Auth.Types as Heroku

import           Fission.User                  as User
import           Fission.LoosePin              as LoosePin
import           Fission.Platform.Heroku.AddOn as Heroku.AddOn

-- Reexport

import           Fission.Internal.Mock.Effect.Types
import           Fission.Internal.Mock.Config.Types

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

instance MonadBasicAuth String (Mock effs) where
  getVerifier = do
    isAuthed <- asks forceAuthed
    return $ BasicAuthCheck \_ ->
      return if isAuthed
                  then Authorized "YUP"
                  else Unauthorized

instance MonadBasicAuth Heroku.Auth (Mock effs) where
  getVerifier = asks herokuVerifier

instance MonadAuth DID (Mock effs) where
  getVerifier = asks didVerifier

instance MonadAuth (Entity User) (Mock effs) where
  getVerifier = asks userVerifier

instance MonadAuth Authorization (Mock effs) where
  getVerifier = asks authVerifier

instance IsMember RunAWS effs => MonadAWS (Mock effs) where
  liftAWS awsAction = do
    Effect.log RunAWS
    env <- newEnv $ FromKeys "FAKE_ACCESS_KEY" "FAKE_SECRET_KEY"
    liftIO . runResourceT $ runAWST env awsAction

instance IsMember CheckTime effs => MonadTime (Mock effs) where
  currentTime = do
    Effect.log CheckTime
    asks now

instance
  ( IsMember RunAWS        effs
  , IsMember UpdateRoute53 effs
  , IsMember ClearRoute53  effs
  )
  => MonadRoute53 (Mock effs) where
  set r url zone nonEmptyTxts ttl = do
    Effect.log UpdateRoute53
    runner <- asks updateRoute53
    return $ runner r url zone nonEmptyTxts ttl

  clear r url _ = do
    Effect.log ClearRoute53
    runner <- asks clearRoute53
    return $ runner r url

instance
  ( IsMember UpdateRoute53 effs
  , IsMember ClearRoute53  effs
  , IsMember SetDNSLink    effs
  , IsMember FollowDNSLink effs
  , IsMember RunAWS        effs
  )
  => MonadDNSLink (Mock effs) where
  set _userID URL {..} _ cid = do
    Effect.log SetDNSLink
    runner <- asks setDNSLink
    return $ runner domainName subdomain cid

  follow _userID toSet _ toFollow = do
    Effect.log $ FollowDNSLink toSet toFollow
    runner <- asks followDNSLink
    return $ runner toSet toFollow

instance IsMember RunLocalIPFS effs => MonadLocalIPFS (Mock effs) where
  runLocal _ _ = do
    Effect.log RunLocalIPFS
    asks localIPFSCall

instance IsMember RunRemoteIPFS effs => MonadRemoteIPFS (Mock effs) where
  runRemote _ = do
    Effect.log RemoteIPFSGeneric
    error "Directly called runRemote"

  ipfsAdd bs = do
    Effect.log $ RemoteIPFSAdd bs
    asks remoteIPFSAdd

  ipfsCat cid = do
    Effect.log $ RemoteIPFSCat cid
    asks remoteIPFSCat

  ipfsPin cid = do
    Effect.log $ RemoteIPFSPin cid
    asks remoteIPFSPin

  ipfsUnpin cid flag = do
    Effect.log $ RemoteIPFSUnpin cid flag
    asks remoteIPFSUnpin

instance MonadReflectiveServer (Mock effs) where
  getHost = Web.Host <$> parseBaseUrl "example.com"

instance IsMember LogMsg effs => MonadLogger (Mock effs) where
  monadLoggerLog _loc _src lvl msg = do
    Effect.log . LogMsg lvl $ toLogStr msg

instance IsMember DestroyHerokuAddOn effs => Heroku.AddOn.Destroyer (Mock effs) where
  destroyByUUID uuid = do
    Effect.log $ DestroyHerokuAddOn uuid
    pure ()

instance IsMember DestroyHerokuAddOn effs => Heroku.AddOn.Retriever (Mock effs) where
  getByUUID uuid = do
    Effect.log $ DestroyHerokuAddOn uuid
    return Nothing

instance IsMember CreateHerokuAddOn effs => Heroku.AddOn.Creator (Mock effs) where
  create uuid _ _ = do
    Effect.log $ CreateHerokuAddOn uuid
    return . Right $ Database.toSqlKey 0

instance IsMember RetrieveUser effs => User.Retriever (Mock effs) where
  getById userId = do
    Effect.log $ GetUserById userId
    return . Just $ Fixture.entity Fixture.user

  getByUsername username = do
    Effect.log $ GetUserByUsername username
    return . Just $ Fixture.entity Fixture.user

  getByPublicKey pk = do
    Effect.log $ GetUserByPublicKey pk
    return . Just $ Fixture.entity Fixture.user

  getByHerokuAddOnId id = do
    Effect.log $ GetUserByHerokuAddOnId id
    pure . Just $ Fixture.entity Fixture.user

  getByEmail email = do
    Effect.log $ GetUserByEmail email
    pure . Just $ Fixture.entity Fixture.user

instance
  ( IsMember CreateHerokuAddOn effs
  , IsMember CreateUser        effs
  , IsMember UpdateRoute53     effs
  )
  => User.Creator (Mock effs) where
  create _ _ _ _ = do
    Effect.log CreateUser
    Effect.log UpdateRoute53
    return . Right $ Database.toSqlKey 0

  createWithPassword _ _ _ _ = do
    Effect.log CreateUser
    Effect.log UpdateRoute53
    return $ Right (Database.toSqlKey 0)

  createWithHeroku uuid _ _ _ _ = do
    Effect.log CreateUser
    Effect.log $ CreateHerokuAddOn uuid
    return . Right $ Database.toSqlKey 0

instance IsMember ModifyUser effs => User.Modifier (Mock effs) where
  updatePassword uID password _ = do
    Effect.log $ ModifyUser uID
    return $ Right password

  updatePublicKey uID newPK _ = do
    Effect.log $ ModifyUser uID
    return $ Right newPK

  addExchangeKey uID key _ = do
    Effect.log $ ModifyUser uID
    return $ Right [key]

  removeExchangeKey uID _ _ = do
    Effect.log $ ModifyUser uID
    return $ Right []

  setData uID _ _ = do
    Effect.log $ ModifyUser uID
    return ok

instance IsMember DestroyUser effs => User.Destroyer (Mock effs) where
  deactivate _ uid = do
    Effect.log $ DestroyUser uid
    return ok

instance IsMember RetrieveLoosePin effs => LoosePin.Retriever (Mock effs) where
  getByUserId uid = do
    Effect.log $ GetLoosePinByUserId uid

    let
      userId = Database.toSqlKey 0
      cid    = IPFS.CID "Qm12345"

    return . pure . Fixture.entity $ LoosePin userId cid Fixture.agesAgo

  getByCids cids = sequence . snd $ foldr folder (0, []) cids
    where
      folder cid (counter, acc) =
        (counter + 1, action cid counter : acc)

      action :: IPFS.CID -> Int64 -> Mock effs (Entity LoosePin)
      action cid rawUserId = do
        let userId = Database.toSqlKey rawUserId
        Effect.log $ GetLoosePinByCID cid
        return . Fixture.entity $ LoosePin userId cid Fixture.agesAgo

instance IsMember CreateLoosePin effs => LoosePin.Creator (Mock effs) where
  create uid cid _ = do
    Effect.log $ CreateLoosePin uid cid
    return . Just $ Database.toSqlKey 0

  createMany uid cids _ = do
    forM_ cids \cid ->
      Effect.log $ CreateLoosePin uid cid

    return cids

instance IsMember DestroyLoosePin effs => LoosePin.Destroyer (Mock effs) where
  destroy userId cid =
    Effect.log $ DestroyLoosePin userId cid

  destroyMany userId cidIds =
    forM_ cidIds \id ->
      Effect.log $ DestroyLoosePinById userId id

instance MonadWebAuth (Mock effs) Authorization where
  getAuth = return Authorization
    { sender   = Right did
    , about    = Fixture.entity Fixture.user
    , potency  = AppendOnly
    , resource = Subset (FissionFileSystem "/test/")
    }
    where
      did = DID
        { publicKey = Ed25519.pk
        , method    = Key
        }
