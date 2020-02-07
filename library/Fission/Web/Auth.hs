module Fission.Web.Auth
  ( Checks
  , authWithContext
  , basic
  , mkAuth
  , handler
  , module Fission.Web.Auth.Class
  , module Fission.Web.Auth.Types
  ) where

import           Fission.Prelude
import           Fission.Models

import qualified Fission.Platform.Heroku.Auth.Types as Heroku

import           Network.Wai
import           Servant
import           Servant.Server.Experimental.Auth

import           Database.Esqueleto

import           Fission.Web.Auth.Class             as Auth
import           Fission.Web.Auth.Types             as Auth
import           Fission.Web.Auth.Token             as Token
import qualified Fission.Web.Auth.Error             as Auth
import qualified Fission.Web.Auth.Token.Basic       as Basic
import           Fission.Web.Auth.Token.Basic.Class as BasicAuth
import qualified Fission.Web.Auth.JWT               as JWT

import qualified Fission.User as User
import           Fission.User.DID.Types

-- Reexport
import           Fission.Web.Auth.Class
import           Fission.Web.Auth.Types

type Checks = '[ AuthHandler    Request DID
               , AuthHandler    Request (Entity User)
               , BasicAuthCheck Heroku.Auth
               ]

-- | Construct an authorization context
mkAuth ::
  ( MonadAuth      DID           m
  , MonadAuth      (Entity User) m
  , MonadBasicAuth Heroku.Auth   m
  )
  => m (Context Checks)
mkAuth = do
  didAuth    <- Auth.getVerifier
  userAuth   <- Auth.getVerifier
  herokuAuth <- BasicAuth.getVerifier
  return <| didAuth
         :. userAuth
         :. herokuAuth
         :. EmptyContext

authWithContext ::
  HasServer api Checks
  => Proxy api
  -> (forall a . m a -> Handler a)
  -> ServerT api m
  -> ServerT api Handler
authWithContext api = hoistServerWithContext api (Proxy @Checks)

-- | Basic auth. Used for Heroku auth check
basic :: ByteString -> ByteString -> BasicAuthCheck ByteString
basic unOK pwOK = BasicAuthCheck (return . check)
  where
    check :: BasicAuthData -> BasicAuthResult ByteString
    check (BasicAuthData username password) =
      if (username == unOK) && (pwOK == password)
         then Authorized username
         else Unauthorized

-- | Higher order auth handler
-- Uses basic auth for "Basic " tokens
-- Uses our custom jwt auth for "Bearer " tokens
handler ::
  ( MonadLogger      m
  , MonadThrow       m
  , MonadTime        m
  , MonadDB        t m
  , MonadThrow     t
  , User.Retriever t
  )
  => Request
  -> m (Entity User)
handler req =
  case Token.get req of
    Nothing                   -> throwM Auth.NoToken
    Just (Auth.Bearer bearer) -> JWT.handler bearer
    Just (Auth.Basic basic')  -> Basic.handler basic'
