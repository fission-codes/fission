module Fission.Web.Server.Handler.User (handlerV_, handlerV2, handlerV3) where

import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.User.Types                     as User

import           Fission.Web.Server.IPFS.DNSLink.Class          as DNSLink
import           Fission.Web.Server.Relay
import           Fission.Web.Server.WNFS.Class

import qualified Fission.Web.Server.Challenge.Creator.Class     as Challenge
import qualified Fission.Web.Server.Challenge.Retriever.Class   as Challenge
import qualified Fission.Web.Server.Challenge.Verifier.Class    as Challenge
import qualified Fission.Web.Server.RecoveryChallenge           as RecoveryChallenge

import qualified Fission.Web.Server.App.Domain                  as App.Domain
import           Fission.Web.Server.Email.Class
import qualified Fission.Web.Server.User.Creator.Class          as User
import qualified Fission.Web.Server.User.Modifier.Class         as User
import qualified Fission.Web.Server.User.Retriever.Class        as User

import qualified Fission.Web.Server.Handler.Relay               as Relay
import qualified Fission.Web.Server.Handler.User.Create         as Create
import qualified Fission.Web.Server.Handler.User.DID            as DID
import qualified Fission.Web.Server.Handler.User.DataRoot       as DataRoot
import qualified Fission.Web.Server.Handler.User.Challenge      as Challenge
import qualified Fission.Web.Server.Handler.User.Email          as Email
import qualified Fission.Web.Server.Handler.User.ExchangeKey    as ExchangeKey
import qualified Fission.Web.Server.Handler.User.Password.Reset as Password.Reset
import qualified Fission.Web.Server.Handler.User.Verify         as Verify
import qualified Fission.Web.Server.Handler.User.WhoAmI         as WhoAmI

handlerV3 ::
  ( User.Modifier               m
  , User.Retriever              m
  , RecoveryChallenge.Retriever m
  , RecoveryChallenge.Destroyer m
  , MonadTime                   m
  , MonadLogger                 m
  , MonadDNSLink                m
  )
  => User.RoutesV3 (AsServerT m)
handlerV3 =
  User.RoutesV3
    { did = genericServerT DID.handlerV3
    }

handlerV2 ::
  ( App.Domain.Initializer      m
  , User.Creator                m
  , User.Modifier               m
  , User.Retriever              m
  , Challenge.Creator           m
  , Challenge.Retriever         m
  , Challenge.Verifier          m
  , RecoveryChallenge.Creator   m
  , RecoveryChallenge.Retriever m
  , RecoveryChallenge.Destroyer m
  , MonadRelayStore             m
  , MonadWNFS                   m
  , MonadTime                   m
  , MonadLogger                 m
  , MonadDNSLink                m
  , MonadEmail                  m
  )
  => User.RoutesV2 (AsServerT m)
handlerV2 =
  User.RoutesV2
    { create       = Create.withDID
    , whoAmI       = genericServerT WhoAmI.handler
    , challenge    = genericServerT Challenge.handler
    , email        = genericServerT Email.handler
    , did          = genericServerT DID.handlerV_
    , linkingRelay = genericServerT Relay.handler
    , dataRoot     = genericServerT DataRoot.handlerV2
    }

handlerV_ ::
  ( App.Domain.Initializer      m
  , User.Creator                m
  , User.Modifier               m
  , User.Retriever              m
  , Challenge.Creator           m
  , Challenge.Retriever         m
  , Challenge.Verifier          m
  , RecoveryChallenge.Creator   m
  , RecoveryChallenge.Retriever m
  , RecoveryChallenge.Destroyer m
  , MonadRelayStore             m
  , MonadWNFS                   m
  , MonadTime                   m
  , MonadLogger                 m
  , MonadDNSLink                m
  , MonadEmail                  m
  )
  => User.RoutesV_ (AsServerT m)
handlerV_ =
  User.RoutesV_
    { create        = genericServerT Create.createV_
    , whoAmI        = genericServerT WhoAmI.handler
    , email         = genericServerT Email.handler
    , did           = genericServerT DID.handlerV_
    , exchangeKeys  = genericServerT ExchangeKey.handler
    , linkingRelay  = genericServerT Relay.handler
    , dataRoot      = genericServerT DataRoot.handlerV_
    , passwordReset = Password.Reset.handler
    , verify        = Verify.handler
    }
