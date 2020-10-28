module Fission.Web.Auth.Token.Bearer
  ( handler
  , module Fission.Web.Auth.Token.Bearer.Types
  ) where

import           Fission.Prelude

import qualified Fission.Web.Error                           as Web.Error

import qualified Fission.Authorization.Session.Types         as Authorization

import           Fission.Web.Auth.Token.JWT.Resolver.Class   as JWT

import           Fission.Web.Auth.Token.UCAN.Fact.Types
import           Fission.Web.Auth.Token.UCAN.Privilege.Types
import           Fission.Web.Auth.Token.UCAN.Types

import           Fission.Authorization.ServerDID

import           Fission.Web.Auth.Token.Bearer.Types         as Bearer


import qualified Fission.User.Retriever                      as User
import qualified Fission.Web.Auth.Token.JWT.Validation       as JWT

import           Fission.Web.Auth.Token.UCAN

-- Reexport

import           Fission.Web.Auth.Token.Bearer.Types

-- | Auth handler for delegated auth
-- Ensures properly formatted token *and does check against DB*
handler ::
  ( m `JWT.Resolves` UCAN Privilege Fact
  , MonadLogger      m
  , MonadThrow       m
  , ServerDID        m
  , MonadTime        m
  , MonadDB        t m
  , User.Retriever t
  )
  => Bearer.Token
  -> m (Authorization.Session)
handler (Bearer.Token jwt rawContent) = do
  void . Web.Error.ensureM $ JWT.check rawContent jwt
  toAuthorization jwt
