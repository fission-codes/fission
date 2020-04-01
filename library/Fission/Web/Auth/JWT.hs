module Fission.Web.Auth.JWT
  ( handler

  -- * Reexport
 
  , module Fission.Web.Auth.JWT.Types
  , module Fission.Web.Auth.JWT.Error
  ) where

import           Fission.Prelude
import           Fission.Key             as Key
import           Fission.Web.Error.Class

import           Fission.Models
import qualified Fission.User as User

import           Fission.Web.Auth.JWT.Types          as JWT
import           Fission.Web.Auth.JWT.Error          as JWT
import qualified Fission.Web.Auth.Token.Bearer.Types as Auth.Bearer

-- Reexport

import           Fission.Web.Auth.JWT.Types
import           Fission.Web.Auth.JWT.Error

-----------------------
import  Fission.Web.Auth.JWT.Validation
------------------------------

handler ::
  ( MonadTime        m
  , MonadLogger      m
  , MonadThrow       m
  , MonadDB        t m
  , MonadThrow     t
  , User.Retriever t
  )
  => Auth.Bearer.Token
  -> m (Entity User)
handler token@(Auth.Bearer.Token rawToken) =
  validateJWT token >>= \case
    Left err -> do
      logWarn $ "Failed login with token " <> rawToken
      throwM err

    Right User.DID {..} -> do
      runDB $ User.getByPublicKey publicKey >>= \case
        Nothing  -> throwM $ toServerError JWT.NoUser
        Just usr -> return usr
