module Fission.Web.Auth
  ( Checks
  , authWithContext
  , basic
  , mkAuth

  -- * Reexports
 
  , module Fission.Web.Auth.Class
  , module Fission.Web.Auth.Types
  ) where

import           Network.Wai
import           Servant
import           Servant.Server.Experimental.Auth
 
import           Fission.Prelude
import           Fission.Authorization
import           Fission.User.DID.Types

import qualified Fission.Platform.Heroku.Auth.Types as Heroku

import           Fission.Web.Auth.Class             as Auth
import           Fission.Web.Auth.Types             as Auth
import           Fission.Web.Auth.Token.Basic.Class as BasicAuth

-- Reexport
 
import           Fission.Web.Auth.Class
import           Fission.Web.Auth.Types

type Checks
  = '[ AuthHandler    Request DID
     , AuthHandler    Request Authorization
     , BasicAuthCheck Heroku.Auth
     ]

-- | Construct an authorization context
mkAuth ::
  ( MonadAuth      DID           m
  , MonadAuth      Authorization m
  , MonadBasicAuth Heroku.Auth   m
  )
  => m (Context Checks)
mkAuth = do
  didAuth         <- Auth.getVerifier
  higherOrderAuth <- Auth.getVerifier
  herokuAuth <- BasicAuth.getVerifier
  return $ didAuth
        :. higherOrderAuth
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

