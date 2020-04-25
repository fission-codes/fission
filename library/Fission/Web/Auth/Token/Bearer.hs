module Fission.Web.Auth.Token.Bearer
  ( handler

  -- * Reexport
 
  , module Fission.Web.Auth.Token.JWT
  , module Fission.Web.Auth.Token.JWT.Error
  , module Fission.Web.Auth.Token.JWT.Validation
  ) where

import           Servant.Server

import           Fission.Prelude

import qualified Fission.Web.Error as Web.Err
import           Fission.Error.NotFound.Types

import           Fission.Authorization

import           Fission.Models
import qualified Fission.User     as User
import qualified Fission.User.DID as DID
 
import qualified Fission.Web.Auth.Token.Bearer.Types as Auth.Bearer

import           Fission.Web.Auth.Token.JWT          as JWT
import           Fission.Web.Auth.Token.JWT.Error    as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver as JWT

-- Reexport

import           Fission.Web.Auth.Token.JWT
import           Fission.Web.Auth.Token.JWT.Error
import           Fission.Web.Auth.Token.JWT.Validation

handler ::
  ( MonadTime        m
  , JWT.Resolver     m
  , ServerDID        m
  , MonadLogger      m
  , MonadThrow       m
  , MonadLogger      m
  , MonadDB        t m
  , MonadThrow     t
  , User.Retriever t
  )
  => Auth.Bearer.Token
  -> m Authorization
handler token@(Auth.Bearer.Token jwt (Just rawContent)) =
  check rawContent jwt >>= \case
    Left err -> do
      logWarn $ "===> Failed login with token !! " <> encode token
      Web.Err.throw err

    Right JWT {claims = Claims {..}} -> do
      runDB (User.getByPublicKey $ DID.publicKey sender) >>= \case
        Nothing ->
          Web.Err.throw $ NotFound @User

        Just about ->
          return Authorization {sender = Right sender, ..}

handler _ = -- should be impossible
  throwM $ err401 { errBody = "Unable to parse JWT" }
