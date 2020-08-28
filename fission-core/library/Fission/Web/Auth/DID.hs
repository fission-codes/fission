module Fission.Web.Auth.DID (handler) where

import           Network.Wai

import           Fission.Prelude
 
import qualified Fission.Web.Error      as Web.Error
import qualified Fission.Web.Auth.Error as Auth

import qualified Fission.Web.Auth.Token              as Token
import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer

import           Fission.Web.Auth.Token.JWT            as JWT
import qualified Fission.Web.Auth.Token.JWT.Validation as JWT
import           Fission.Web.Auth.Token.JWT.Resolver   as JWT

import           Fission.User.DID.Types
import           Fission.Authorization.ServerDID

-- | Auth handler for registering DIDs
-- Ensures properly formatted token but *does not check against DB*
handler ::
  ( JWT.Resolver m
  , ServerDID    m
  , MonadLogger  m
  , MonadThrow   m
  , MonadTime    m
  )
  => Request
  -> m DID
handler req =
  case Token.get req of
    Right (Token.Bearer token@(Bearer.Token jwt rawContent)) ->
      JWT.check rawContent jwt >>= \case
        Left err -> do
          logWarn $ "Failed registration with token " <> textDisplay token
          Web.Error.throw err

        Right JWT.JWT {claims = JWT.Claims {sender}} ->
          return sender

    Right token@(Token.Basic _) -> do
      logWarn $ "Attempted registration with basic auth" <> textDisplay token
      Web.Error.throw Auth.Unauthorized

    Left err ->
      Web.Error.throw err
     
