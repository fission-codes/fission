module Fission.Web.Server.Auth
  ( Checks
  , authWithContext
  , basic
  , mkAuth

  -- * Reexports

  , module Fission.Web.Server.Auth.Class
  , module Fission.Web.API.Auth.Types
  ) where

import           Network.Wai
import           Servant
import           Servant.Server.Experimental.Auth

import           Fission.Prelude

import           Web.DID.Types

import           Fission.Web.API.Auth.Types                as Auth
import qualified Fission.Web.API.Heroku.Auth.Types         as Heroku

import           Fission.Web.Server.Auth.Class             as Auth
import           Fission.Web.Server.Auth.Token.Basic.Class as BasicAuth
import           Fission.Web.Server.Authorization.Types

-- Reexport

import           Fission.Web.API.Auth.Types
import           Fission.Web.Server.Auth.Class

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
  herokuAuth      <- BasicAuth.getVerifier
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

