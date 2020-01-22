module Fission.Internal.Mock.Config
  ( module Fission.Internal.Mock.Config.Types
  , defaultConfig
  ) where

import Fission.Internal.Mock.Config.Types


import           Servant


import Fission.Internal.Fixture.Time as Fixture
import           Fission.Internal.Fixture.Entity as Fixture
import           Fission.Internal.Fixture.User   as Fixture

import Fission.Internal.Orphanage.CID ()
import Fission.Internal.Orphanage.Serilaized ()

import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude

import qualified Fission.Platform.Heroku.Auth.Types as Heroku

import Network.AWS.Route53
import           Fission.URL.Types as URL

defaultConfig :: Config
defaultConfig = Config
  { setDNSLink     = \_ _   -> Right <| DomainName "example.com"
  , updateRoute53  = \_ _ _ -> Right <| changeResourceRecordSetsResponse 200 <| changeInfo "ciId" Insync agesAgo
  , now            = agesAgo
  , linkedPeers    = pure <| IPFS.Peer "ipv4/fakepeeraddress"
  , userVerifier   = BasicAuthCheck \_ -> pure <| Authorized <| Fixture.entity Fixture.user
  , herokuVerifier = BasicAuthCheck \_ -> pure <| Authorized <| Heroku.Auth "FAKE HEROKU"
  , localIPFSCall  = Right "Qm1234567890"
  , forceAuthed    = True
  }
