module Fission.Web.Auth
  ( Checks
  , authWithContext
  , basic
  , mkAuth
  , module Fission.Web.Auth.Class
  , module Fission.Web.Auth.Types
  ) where

import           Servant
import           Database.Esqueleto

import           Fission.Models
import           Fission.Platform.Heroku.Auth.Types as Heroku
import           Fission.Prelude
import           Fission.Web.Auth.Class
import           Fission.Web.Auth.Types

type Checks = '[BasicAuthCheck (Entity User), BasicAuthCheck Heroku.Auth]

-- | Construct an authorization context
mkAuth ::
  ( MonadAuth (Entity User) m
  , MonadAuth Heroku.Auth   m
  )
  => m (Context Checks)
mkAuth = do
  userAuth   <- verify
  herokuAuth <- verify
  return (userAuth :. herokuAuth :. EmptyContext)

authWithContext ::
  HasServer api Checks
  => Proxy api
  -> (forall a . m a -> Handler a)
  -> ServerT api m
  -> ServerT api Handler
authWithContext api = hoistServerWithContext api (Proxy @Checks)

basic :: ByteString -> ByteString -> BasicAuthCheck ByteString
basic unOK pwOK = BasicAuthCheck (return . check)
  where
    check :: BasicAuthData -> BasicAuthResult ByteString
    check (BasicAuthData username password) =
      if (username == unOK) && (pwOK == password)
         then Authorized username
         else Unauthorized
