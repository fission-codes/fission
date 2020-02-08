module Fission.Web.User.Email
  ( API
  , server
  ) where

import           Servant

import           Fission.Prelude
import           Fission.Models

import qualified Fission.User as User

import           Fission.User.Username.Types
import           Fission.User.Email.Types

import           Database.Esqueleto

type API = QueryParam "username" Username
        :> QueryParam "email"    Email
        :> Get '[JSON] Bool

server ::
  ( MonadThrow       m
  , MonadDB        t m
  , MonadThrow     t
  , User.Retriever t
  )
  => ServerT API m
server (Just username) (Just reqEmail) =
  runDB <| User.getByUsername username >>= \case
    Nothing -> throwM err401
    Just (Entity _ User { userEmail }) -> return <| maybe False (== reqEmail) userEmail

server _ _ = throwM err404
