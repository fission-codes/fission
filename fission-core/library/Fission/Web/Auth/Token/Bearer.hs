module Fission.Web.Auth.Token.Bearer
  ( handler
  , module Fission.Web.Auth.Token.Bearer.Types
  ) where

-- Reexport

import           Fission.Web.Auth.Token.Bearer.Types

-- | Auth handler for delegated auth
-- Ensures properly formatted token *and does check against DB*
handler ::
  ( MonadLogger      m
  , MonadThrow       m
  , Resolver         m
  , ServerDID        m
  , MonadTime        m
  , MonadDB        t m
  , User.Retriever t
  )
  => Bearer.Token
  -> m (Authorization [Resource])
handler (Bearer.Token jwt rawContent) = do
  void . Web.Error.ensureM $ JWT.check rawContent jwt
  toAuthorization jwt
