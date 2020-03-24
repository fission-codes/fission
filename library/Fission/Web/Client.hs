module Fission.Web.Client
  ( request
  , sigClient
  , sigClient'
  , basicClient
  , registerClient
  , is404
  , module Fission.Web.Client.Types
  , module Fission.Web.Client.Class
  ) where

import           Fission.Prelude

import           Servant
import           Servant.Client
import           Servant.Client.Core

import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Status

import           Fission.Web.Client.Types
import           Fission.Web.Client.Class
import           Fission.Web.Client.BasicAuth as Auth

import           Fission.Web.Client.JWT as JWT
import           Fission.Web.Auth.Types as Auth

request :: HTTP.Manager -> BaseUrl -> ClientM a -> IO (Either ClientError a)
request manager url query = runClientM query <| mkClientEnv manager url

sigClient ::
  ( HasClient ClientM api
  , Client ClientM api ~ (AuthenticatedRequest Auth.HigherOrder -> a -> ClientM b)
  )
  => Proxy api
  -> a
  -> ClientM b
sigClient p x = JWT.getSigAuth >>= flip (client p) x

sigClient' ::
  ( HasClient ClientM api
  , Client ClientM api ~ (AuthenticatedRequest Auth.HigherOrder -> ClientM b)
  )
  => Proxy api
  -> ClientM b
sigClient' p = JWT.getSigAuth >>= client p

basicClient ::
  ( HasClient ClientM api
  , Client ClientM api ~ (AuthenticatedRequest Auth.HigherOrder -> a -> ClientM b)
  )
  => Proxy api
  -> BasicAuthData
  -> a
  -> ClientM b
basicClient p auth x = (client p) (Auth.getBasicAuth auth) x

registerClient ::
  ( HasClient ClientM api
  , Client ClientM api ~ (AuthenticatedRequest Auth.RegisterDid -> a -> ClientM b)
  )
  => Proxy api
  -> a
  -> ClientM b
registerClient p x = JWT.getRegisterAuth >>= flip (client p) x

is404 :: ClientError -> Bool
is404 (FailureResponse _ resp) = responseStatusCode resp == status404
is404 _ = False
