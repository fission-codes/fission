module Fission.Internal.Mock.Config
  ( module Fission.Internal.Mock.Config.Types
  , defaultConfig
  ) where

import           Network.AWS.Route53
import qualified Network.IPFS.Types as IPFS
import           Network.IPFS.File.Types as File
import           Network.IPFS.Client.Pin as IPFS.Client

import           Servant

import           Fission.Prelude
import qualified Fission.Platform.Heroku.Auth.Types as Heroku
import           Fission.URL.Types as URL

import           Fission.Internal.Fixture.Time as Fixture
import           Fission.Internal.Fixture.Entity as Fixture
import           Fission.Internal.Fixture.User   as Fixture
import           Fission.Internal.Mock.Config.Types

import           Fission.Internal.Orphanage.CID ()
import           Fission.Internal.Orphanage.Serilaized ()

defaultConfig :: Config
defaultConfig = Config
  { now             = agesAgo
  , linkedPeers     = pure <| IPFS.Peer "ipv4/fakepeeraddress"
  , userVerifier    = BasicAuthCheck \_ -> pure . Authorized <| Fixture.entity Fixture.user
  , herokuVerifier  = BasicAuthCheck \_ -> pure . Authorized <| Heroku.Auth "FAKE HEROKU"
  , localIPFSCall   = Right "Qm1234567890"
  , forceAuthed     = True
  , remoteIPFSAdd   = Right <| IPFS.CID "Qm1234567890"
  , remoteIPFSCat   = Right <| File.Serialized "hello world"
  , remoteIPFSPin   = Right <| IPFS.Client.Response [IPFS.CID "Qmfhajhfjka"]
  , remoteIPFSUnpin = Right <| IPFS.Client.Response [IPFS.CID "Qmhjsdahjhkjas"]
  , setDNSLink      = \_ _   -> Right <| DomainName "example.com"
  , updateRoute53   = \_ _ _ ->
      agesAgo
        |> changeInfo "ciId" Insync
        |> changeResourceRecordSetsResponse 200
        |> Right
  }
