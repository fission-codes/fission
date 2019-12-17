module Fission.Web.Auth.DID (handler) where

import           Fission.Prelude

import           Network.Wai

import           Fission.Web.Auth.Types as Auth
import           Fission.Web.Auth.Utils
import qualified Fission.Web.Auth.Error as Auth
import qualified Fission.Web.Auth.JWT       as JWT

import           Fission.User.DID.Types

-- | Auth handler for registering DIDs
-- Ensures properly formatted token but does not check against DB
handler ::
  ( MonadIO     m
  , MonadLogger m
  , MonadThrow  m
  , MonadTime   m
  )
  => Request
  -> m DID
handler req = 
  case getToken req of
    Auth.Bearer token -> 
      JWT.validateJWT token >>= \case
        Left err -> do
          logWarn <| "Failed registration with token " <> Auth.unBearer token
          throwM err
        Right did' -> return did'

    _ -> throwM Auth.NoToken
