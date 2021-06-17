module Fission.Web.Server.Handler.User.Email (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.Email.Types                   as API

import qualified Fission.Web.Server.Challenge.Retriever.Class       as Challenge
import qualified Fission.Web.Server.Challenge.Verifier.Class        as Challenge
import qualified Fission.Web.Server.RecoveryChallenge.Creator.Class as RecoveryChallenge
import           Fission.Web.Server.User.Retriever.Class            as User

import           Fission.Web.Server.Email.Class

import qualified Fission.Web.Server.Handler.User.Email.Recover      as Recover
import qualified Fission.Web.Server.Handler.User.Email.Resend       as Resend
import qualified Fission.Web.Server.Handler.User.Email.Verify       as Verify

handler ::
  ( Challenge.Retriever       m
  , Challenge.Verifier        m
  , RecoveryChallenge.Creator m
  , User.Retriever            m
  , MonadThrow                m
  , MonadLogger               m
  , MonadEmail                m
  , MonadTime                 m
  )
  => ServerT API.Email m
handler = Verify.handler
     :<|> Resend.handler
     :<|> Recover.handler
