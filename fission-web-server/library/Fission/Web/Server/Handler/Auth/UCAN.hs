module Fission.Web.Server.Handler.Auth.UCAN (handler) where

import           Servant
import           Servant.Server.Generic

import           Web.UCAN.Resolver                   as Proof
import           Web.UCAN.Types
import qualified Web.UCAN.Validation                 as UCAN

import           Fission.Prelude

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer

import qualified Fission.Web.API.Auth.UCAN.Types     as UCAN

import qualified Fission.Web.Server.Error            as Web

handler ::
  ( MonadTime      m
  , MonadLogger    m
  , MonadThrow     m
  , Proof.Resolver m
  )
  => UCAN.Routes (AsServerT m)
handler = UCAN.Routes { verify }
  where
    verify (Bearer.BareToken Bearer.Token {rawContent, jwt}) ignoreTime = do
      let
        UCAN {claims = Claims {receiver, exp}} = jwt

      if ignoreTime
        then Web.ensureM $ UCAN.check'         rawContent jwt exp
        else Web.ensureM $ UCAN.check receiver rawContent jwt

      return NoContent
