module Fission.Web.Auth.JWT
  ( handler

  -- * Reexport
 
  , module Fission.Web.Auth.JWT.Types
  , module Fission.Web.Auth.JWT.Error
  , module Fission.Web.Auth.JWT.Validation
  ) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

import           Fission.Models
import qualified Fission.User as User

import           Fission.Web.Auth.JWT.Types          as JWT
import           Fission.Web.Auth.JWT.Error          as JWT
import qualified Fission.Web.Auth.Token.Bearer.Types as Auth.Bearer
 
import qualified Fission.Web.Auth.JWT.Proof.Resolver as JWT.Proof

-- Reexport

import           Fission.Web.Auth.JWT.Types
import           Fission.Web.Auth.JWT.Error
import           Fission.Web.Auth.JWT.Validation

handler ::
  ( MonadTime          m
  , JWT.Proof.Resolver m
  , MonadLogger        m
  , MonadThrow         m
  , MonadDB        t   m
  , MonadThrow     t
  , User.Retriever t
  )
  => Auth.Bearer.Token
  -> m (Entity User)
handler token@(Auth.Bearer.Token jwt (Just rawContent)) =
  check rawContent jwt >>= \case
    Left err -> do
      logWarn $ "Failed login with token " <> encode token
      throwM err

    Right JWT {claims = Claims {sender = User.DID {publicKey}}} -> do
      runDB $ User.getByPublicKey publicKey >>= \case
        Nothing  -> throwM $ toServerError JWT.NoUser
        Just usr -> return usr
 
handler _ = -- should be impossible
  throwM $ err500 { errBody = "Unable to parse JWT" }
