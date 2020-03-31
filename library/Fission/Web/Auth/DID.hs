module Fission.Web.Auth.DID (handler) where

import           Fission.Prelude

import           Network.Wai

import           Fission.Web.Auth.Types as Auth
import           Fission.Web.Auth.Token as Token
import qualified Fission.Web.Auth.Error as Auth
import qualified Fission.Web.Auth.JWT   as JWT

import           Fission.User.DID.Types

import qualified Fission.Web.Auth.Token.Bearer.Types as Auth.Bearer

-- | Auth handler for registering DIDs
-- Ensures properly formatted token but does not check against DB
handler ::
  ( MonadIO     m
  , MonadLogger m
  , MonadThrow  m
  , MonadTime   m
  )
  => Request
  -> m PublicKey
handler req =
  case Token.get req of
    Just (Auth.Bearer token@(Auth.Bearer.Token bearer)) ->
      JWT.validateJWT token >>= \case
        Left err -> do
          logWarn <| "Failed registration with token " <> bearer
          throwM err

        Right did' ->
          return did'

    _ -> throwM Auth.NoToken
