module Fission.Web.Server.Handler.Auth.UCAN (handler) where

import qualified Network.HTTP.Client                   as HTTP
import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.Auth.Token.Bearer.Types   as Bearer

import           Fission.Web.Auth.Token.JWT.Resolver   as Proof
import           Fission.Web.Auth.Token.JWT.Types
import qualified Fission.Web.Auth.Token.JWT.Validation as UCAN

import qualified Fission.Web.API.Auth.UCAN.Types       as UCAN

import qualified Fission.Web.Server.Error              as Web

handler ::
  ( MonadTime      m
  , MonadLogger    m
  , MonadThrow     m
  , MonadIO        m
  , Proof.Resolver m
  )
  => UCAN.Routes (AsServerT m)
handler = UCAN.Routes { verify }
  where
    verify (Bearer.BareToken Bearer.Token {rawContent, jwt}) ignoreTime = do
      let
        JWT {claims = Claims {receiver, exp}} = jwt

      manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings

      if ignoreTime
        then Web.ensureM $ UCAN.checkWithION manager receiver rawContent jwt exp
        else Web.ensureM $ UCAN.check manager receiver rawContent jwt

      return NoContent
