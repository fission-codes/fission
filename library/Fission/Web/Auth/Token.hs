module Fission.Web.Auth.Token
  ( get
  , handler
  , module Fission.Web.Auth.Token.Types
  ) where

import           Network.Wai
import           Servant.API

import           Fission.Prelude
import           Fission.Authorization
import qualified Fission.User as User

import qualified Fission.Web.Auth.Error       as Auth
import qualified Fission.Web.Auth.Token.Basic as Basic
import qualified Fission.Web.Auth.Token.UCAN  as UCAN

import           Fission.Web.Auth.Token.Types
import qualified Fission.Web.Auth.Token.JWT.Resolver as JWT

-- | Higher order auth handler
--   Uses basic auth for "Basic" tokens
--   Uses our custom JWT auth for "Bearer" tokens
handler ::
  ( JWT.Resolver     m
  , ServerDID        m
  , MonadLogger      m
  , MonadThrow       m
  , MonadTime        m
  , MonadDB        t m
  , User.Retriever t
  )
  => Request
  -> m Authorization
handler req =
  case get req of
    Just (Bearer bearer) -> UCAN.handler bearer
    Just (Basic  basic') -> Basic.handler basic'
    Nothing              -> throwM Auth.NoToken

get :: Request -> Maybe Token
get req = do
  auth <- lookup "Authorization" headers <|> lookup "authorization" headers

  case parseHeader auth of
    Right token -> Just token
    Left  _     -> Nothing

  where
    headers = requestHeaders req
