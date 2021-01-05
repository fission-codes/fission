module Fission.Web.Server.Auth.Token
  ( get
  , handler
  , module Fission.Web.Server.Auth.Token.Types
  ) where

import           Network.Wai
import           Servant.API

import           Fission.Prelude

import           Fission.Authorization
import qualified Fission.User                               as User

import qualified Fission.Web.Server.Auth.Error              as Auth
import qualified Fission.Web.Server.Auth.Token.Basic        as Basic
import qualified Fission.Web.Server.Auth.Token.UCAN         as UCAN
import qualified Fission.Web.Server.Error                   as Web.Error

import qualified Fission.Web.Server.Auth.Token.JWT.Resolver as JWT
import           Fission.Web.Server.Auth.Token.Types

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
    Right (Bearer bearer) -> UCAN.handler bearer
    Right (Basic  basic') -> Basic.handler basic'
    Left  err             -> Web.Error.throw err

get :: Request -> Either Auth.Error Token
get req =
  case lookup "Authorization" headers <|> lookup "authorization" headers of
    Nothing ->
      Left Auth.NoToken

    Just auth ->
      case parseHeader auth of
        Left errMsg -> Left $ Auth.CannotParse errMsg
        Right token -> Right token

  where
    headers = requestHeaders req
