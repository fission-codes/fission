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

import           Servant
import           Servant.Server.Experimental.Auth

import           Database.Esqueleto

import           Network.Wai

import           Fission.Web.Auth.Class
import           Fission.Web.Auth.Types
import           Fission.Web.Auth.Types as Auth
import           Fission.Web.Auth.Utils
import qualified Fission.Web.Auth.Error as Auth
import qualified Fission.Web.Auth.Basic as Basic
import qualified Fission.Web.Auth.JWT       as JWT

import qualified Fission.User as User
import           Fission.User.DID.Types

type Checks = '[AuthHandler Request DID, AuthHandler Request (Entity User), BasicAuthCheck Heroku.Auth]

-- | Construct an authorization context
mkAuth ::
  ( MonadAuth (AuthHandler Request DID)           m
  , MonadAuth (AuthHandler Request (Entity User)) m
  , MonadAuth (BasicAuthCheck Heroku.Auth)        m
  )
  => m (Context Checks)
mkAuth = do
  didAuth    <- getVerifier
  userAuth   <- getVerifier
  herokuAuth <- getVerifier
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
  , User.Retriever t
  )
  => Request
  -> m (Entity User)
handler req =
  case getToken req of
    Auth.None -> throwM Auth.NoToken
    Auth.Basic basic' -> Basic.handler basic'
    Auth.Bearer bearer -> JWT.handler bearer
