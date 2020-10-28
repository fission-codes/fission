module Fission.Internal.Mock.Config
  ( module Fission.Internal.Mock.Config.Types
  , defaultConfig
  ) where

import           Network.AWS.Route53
import           Network.IPFS.Client.Pin               as IPFS.Client
import           Network.IPFS.File.Types               as File
import qualified Network.IPFS.Types                    as IPFS

import           Servant
import           Servant.Server.Experimental.Auth

import qualified Fission.Platform.Heroku.Auth.Types    as Heroku
import           Fission.Prelude

import           Fission.User.DID.Types

import qualified Fission.Authorization                 as Authorization

import           Fission.URL.Types                     as URL

import           Fission.Internal.Fixture.Entity       as Fixture
import           Fission.Internal.Fixture.Key.Ed25519  as Fixture.Ed25519
import           Fission.Internal.Fixture.Time         as Fixture
import           Fission.Internal.Fixture.User         as Fixture

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

  , getRoute53      = \_ _ ->
      Right $ resourceRecordSet "mock" Txt
  }

authZ :: m (Authorization.Session)
-- authZ :: Monad m => m (Authorization.Session)
authZ = undefined -- FIXME!!
-- authZ = return Authorization.Session
--     { sender     = Right did
--     , about      = Fixture.entity Fixture.user
--     , privileges = Subset [] --FIXME add several
--     }
--     where
--       did = DID
--         { publicKey = Fixture.Ed25519.pk
--         , method    = Key
--         }
