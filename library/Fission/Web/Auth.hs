module Fission.Web.Auth
  ( Checks
  , authWithContext
  , basic
  , mkAuth
  ) where

import           Servant
import           Database.Esqueleto

import           Fission.Models
import           Fission.Platform.Heroku as Heroku
import           Fission.Prelude
import qualified Fission.User.Authorizer.Class as User

type Checks = '[BasicAuthCheck (Entity User), BasicAuthCheck ByteString]

-- | Construct an authorization context
mkAuth ::
  ( Heroku.Authorizer m
  , User.Authorizer   m
  )
  => m (Context Checks)
mkAuth = do
  userAuth   <- User.verify
  herokuAuth <- Heroku.verify
  return (userAuth :. herokuAuth :. EmptyContext)

authWithContext
  :: HasServer api Checks
  => Proxy api
  -> (forall a . m a -> Handler a)
  -> ServerT api m
  -> ServerT api Handler
authWithContext api = hoistServerWithContext api (Proxy @Checks)

basic :: ByteString -> ByteString -> BasicAuthCheck ByteString
basic unOK pwOK = BasicAuthCheck (return . check')
  where
    check' :: BasicAuthData -> BasicAuthResult ByteString
    check' (BasicAuthData username password) =
      if (username == unOK) && (pwOK == password)
         then Authorized username
         else Unauthorized
