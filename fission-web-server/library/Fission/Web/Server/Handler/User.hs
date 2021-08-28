{-# LANGUAGE AllowAmbiguousTypes #-}

module Fission.Web.Server.Handler.User (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.Types                     as API

import           Fission.Web.Server.IPFS.DNSLink.Class          as DNSLink
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

import qualified Fission.Web.Server.Handler.User.Create         as Create
import qualified Fission.Web.Server.Handler.User.DID            as DID
import qualified Fission.Web.Server.Handler.User.DataRoot       as DataRoot
import qualified Fission.Web.Server.Handler.User.Email          as Email
import qualified Fission.Web.Server.Handler.User.ExchangeKey    as ExchangeKey
import qualified Fission.Web.Server.Handler.User.Password.Reset as Password.Reset
import qualified Fission.Web.Server.Handler.User.Verify         as Verify
import qualified Fission.Web.Server.Handler.User.WhoAmI         as WhoAmI

handler ::
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
  , MonadWNFS                   m
  , MonadTime                   m
  , MonadLogger                 m
  , MonadDNSLink                m
  , MonadEmail                  m
  )
  => ServerT API.User m
handler = Create.create
     :<|> WhoAmI.handler
     :<|> Verify.handler
     :<|> Email.handler
     :<|> DID.handler
     :<|> ExchangeKey.handler
     :<|> DataRoot.handler
     :<|> Password.Reset.handler
