module Fission.Internal.Mock.Config
  ( module Fission.Internal.Mock.Config.Types
  , defaultConfig
  ) where

import           Network.AWS.Route53
import qualified Network.IPFS.Types as IPFS
import           Network.IPFS.File.Types as File
import           Network.IPFS.Client.Pin as IPFS.Client

import           Servant
import           Servant.Server.Experimental.Auth

import           Fission.Prelude
import qualified Fission.Platform.Heroku.Auth.Types as Heroku
 
import           Fission.User.DID.Types

import           Fission.Authorization.Types
import           Fission.Authorization.Potency.Types
 
import           Fission.Web.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
 
import           Fission.URL.Types as URL

import           Fission.Internal.Fixture.Time        as Fixture
import           Fission.Internal.Fixture.Entity      as Fixture
import           Fission.Internal.Fixture.Key.Ed25519 as Fixture.Ed25519
import           Fission.Internal.Fixture.User        as Fixture

import           Fission.Internal.Mock.Config.Types

import           Fission.Internal.Orphanage.CID        ()
import           Fission.Internal.Orphanage.Serilaized ()

defaultConfig :: Config
defaultConfig = Config
  { now             = agesAgo
  , linkedPeers     = pure $ IPFS.Peer "ipv4/fakepeeraddress"
  , didVerifier     = mkAuthHandler \_ ->
      return $ DID
        { publicKey = pk
        , method    = Key
        }
  , userVerifier    = mkAuthHandler  \_ -> pure $ Fixture.entity Fixture.user
  , authVerifier    = mkAuthHandler  \_ -> authZ
  , herokuVerifier  = BasicAuthCheck \_ -> pure . Authorized $ Heroku.Auth "FAKE HEROKU"
  , localIPFSCall   = Right "Qm1234567890"
  , forceAuthed     = True
  , remoteIPFSAdd   = Right $ IPFS.CID "Qm1234567890"
  , remoteIPFSCat   = Right $ File.Serialized "hello world"
  , remoteIPFSPin   = Right $ IPFS.Client.Response [IPFS.CID "Qmfhajhfjka"]
  , remoteIPFSUnpin = Right $ IPFS.Client.Response [IPFS.CID "Qmhjsdahjhkjas"]
  , setDNSLink      = \_ _ _ -> Right $ URL (DomainName "example.com") Nothing
  , followDNSLink   = \_ _   -> Right ()
  , getBaseDomain   = DomainName "example.com"
  , clearRoute53    = \_ _ ->
      Right . changeResourceRecordSetsResponse 200 $ changeInfo "ciId" Insync agesAgo

  , updateRoute53   = \_ _ _ _ _ ->
      Right . changeResourceRecordSetsResponse 200 $ changeInfo "ciId" Insync agesAgo
  }

authZ :: Monad m => m Authorization
authZ = return Authorization
    { sender   = Right did
    , about    = Fixture.entity Fixture.user
    , potency  = AppendOnly
    , resource = Subset $ FissionFileSystem "/test/"
    }
    where
      did = DID
        { publicKey = Fixture.Ed25519.pk
        , method    = Key
        }
