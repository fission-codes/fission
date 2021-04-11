module Fission.Web.Server.Handler.Auth.UCAN.Verify (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.Auth.Token.Bearer.Types    as Bearer

import           Fission.Web.Auth.Token.JWT.Resolver    as Proof
import           Fission.Web.Auth.Token.JWT.Types
import qualified Fission.Web.Auth.Token.JWT.Validation  as UCAN

import qualified Fission.Web.API.Auth.UCAN.Verify.Types as API.UCAN

import qualified Fission.Web.Server.Error               as Web

handler ::
  ( MonadTime      m
  , MonadLogger    m
  , MonadThrow     m
  , Proof.Resolver m
  )
  => ServerT API.UCAN.Verify m
handler (Bearer.BareToken Bearer.Token {rawContent, jwt}) ignoreTime = do
  let
    JWT {claims = Claims {receiver, exp}} = jwt

  if ignoreTime
    then Web.ensureM $ UCAN.check'         rawContent jwt exp
    else Web.ensureM $ UCAN.check receiver rawContent jwt

  return NoContent
