module Fission.Web.Server.Auth.DID (handler) where

import           Network.Wai

import           Web.DID.Types

import           Web.Ucan.Resolver                   as Ucan
import qualified Web.Ucan.Types                      as Ucan
import qualified Web.Ucan.Validation                 as Ucan

import           Fission.Prelude

import           Fission.Authorization.ServerDID

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer

import qualified Fission.Web.Server.Auth.Error       as Auth
import qualified Fission.Web.Server.Error            as Web.Error

import qualified Fission.Web.Server.Auth.Token       as Token

-- | Auth handler for registering DIDs
-- Ensures properly formatted token but *does not check against DB*
handler ::
  ( Ucan.Resolver m
  , ServerDID    m
  , MonadLogger  m
  , MonadThrow   m
  , MonadTime    m
  )
  => Request
  -> m DID
handler req =
  case Token.get req of
    Right (Token.Bearer token@(Bearer.Token jwt rawContent)) -> do
      serverDID <- getServerDID
      Ucan.check serverDID rawContent jwt >>= \case
        Left err -> do
          logWarn $ "Failed registration with token " <> textDisplay token
          Web.Error.throw err

        Right Ucan.Ucan {claims = Ucan.Claims {sender}} ->
          return sender

    Right token@(Token.Basic _) -> do
      logWarn $ "Attempted registration with basic auth" <> textDisplay token
      Web.Error.throw Auth.Unauthorized

    Left err ->
      Web.Error.throw err
