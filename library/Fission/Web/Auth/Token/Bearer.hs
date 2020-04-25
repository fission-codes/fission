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
  , MonadDB        t m
  , User.Retriever t
  )
  => Auth.Bearer.Token
  -> m Authorization
handler Auth.Bearer.Token {..} =
  case rawContent of
    Nothing ->
      throwM $ err401 { errBody = "Unable to parse JWT" }

    Just encoded ->
      check encoded jwt >>= \case
        Left err ->
          Web.Err.throw err

        Right JWT {claims = Claims {..}} -> do
          runDB (User.getByPublicKey $ DID.publicKey sender) >>= \case
            Nothing ->
              Web.Err.throw $ NotFound @User

            Just about ->
              return Authorization {sender = Right sender, ..}
