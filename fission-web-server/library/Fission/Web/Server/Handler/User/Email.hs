module Fission.Web.Server.Handler.User.Email (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.Email.Types                     as API

import qualified Fission.Web.Server.Challenge.Retriever.Class   as Challenge
import qualified Fission.Web.Server.Challenge.Verifier.Class    as Challenge

import           Fission.Web.Server.Email.Class

import qualified Fission.Web.Server.Handler.User.Email.Verify   as Verify
import qualified Fission.Web.Server.Handler.User.Email.Resend   as Resend

handler ::
  ( Challenge.Retriever    m
  , Challenge.Verifier     m
  , MonadThrow             m
  , MonadLogger            m
  , MonadEmail             m
  )
  => ServerT API.Email m
handler = Verify.handler
     :<|> Resend.handler
