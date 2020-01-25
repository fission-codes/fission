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

type API = Capture "username" Username
            :> Get '[PlainText] Email

server ::
  ( MonadThrow       m
  , MonadDB        t m
  , User.Retriever t
  )
  => ServerT API m
server (Username username) = do
  mayUser <- runDB <| User.getByUsername username

  case mayUser of
    Nothing -> throwM err404
    Just (Entity _ user) -> 
      return . Email <| maybe "" identity <| userEmail user
