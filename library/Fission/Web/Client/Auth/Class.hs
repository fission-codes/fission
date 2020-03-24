module Fission.Web.Client.Auth.Class
  ( MonadAuthedClient
  , MonadWebAuth (..)
  , module Fission.Web.Client
  ) where

import           Fission.Prelude
import           Servant
import           Fission.Web.Client

type MonadAuthedClient m = (MonadWebAuth m, MonadWebClient m)

class Monad m => MonadWebAuth m where
  getAuth :: m BasicAuthData
