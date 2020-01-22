module Fission.Internal.Mock.Types
  ( module Test.Fission.Mock.Session.Types
  , Mock           (..)

  -- Effects

  , RunDB          (..)
  , GetVerifier    (..)
  , GetLinkedPeers (..)
  , CheckTime      (..)
  , SetDNSLink     (..)
  ) where

import GHC.TypeLits (Symbol)

import Fission.Environment ((.!~))

import           Network.AWS
import           Network.AWS.Route53

import           Control.Monad.Writer
import           Data.Generics.Product
import           Database.Esqueleto

import qualified Network.IPFS.Types as IPFS
import           Servant

import           Fission.Internal.Mock.Effect as Effect

import           Fission.Prelude
import           Fission.IPFS.Linked.Class
import           Fission.Web.Auth.Class
import           Fission.Models
import Fission.IPFS.DNSLink.Class

import Fission.AWS.Route53.Class

import Fission.Internal.Fixture.Time as Fixture
import Fission.URL.DomainName.Types
import qualified Fission.Platform.Heroku.Auth.Types as Heroku

import Fission.Internal.Mock.Session.Types

import qualified Fission.Internal.Mock.Config.Types as Mock

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
            , MonadReader ctx
            , MonadIO
            )

instance IsMember RunDB effs => MonadDB (Mock effs) (Mock effs) where
  runDB runner = do
    Effect.log RunDB
    runner

instance IsMember GetLinkedPeers effs => MonadLinkedIPFS (Mock effs) where
  getLinkedPeers = do
    peerList <- asks getLinkedPeers
    Effect.log (GetLinkedPeers peerList)
    return peerList

instance IsMember GetVerifier effs => MonadAuth Text (Mock effs) where
  verify = do
    Effect.log GetVerifier
    verifier <- asks anyVerifier
    return (verifier "YUP")

instance IsMember GetVerifier effs => MonadAuth (Entity User) (Mock effs) where
  verify = do
    Effect.log GetVerifier
    asks userVerifier

instance IsMember GetVerifier effs => MonadAuth Heroku.Auth (Mock effs) where
  verify = do
    Effect.log GetVerifier
    asks herokuVerifier

instance IsMember CheckTime effs => MonadTime (Mock effs) where
  currentTime = do
    Effect.log CheckTime
    asks now

instance IsMember UpdateRoute53 effs => MonadRoute53 (Mock effs) where
  update = do
    Effect.log UpdateRoute53
    asks updateRoute53

instance IsMember SetDNSLink effs => MonadDNSLink (Mock effs) where
  set = do
    Effect.log SetDNSLink
    asks setDNSLink
