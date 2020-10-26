module Fission.Web.User.UpdatePublicKey
  ( API
  , server
  ) where

import           Servant

import           Fission.Prelude

import qualified Fission.Authorization                      as Authorization
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import qualified Fission.Web.Error                          as Web.Error

import qualified Fission.Key                                as Key
import qualified Fission.User                               as User

type API
  =  Summary "Update Public Key"
  :> Description "Set currently authenticated user's root public key to another one"
  :> ReqBody '[JSON] Key.Public
  :> Patch   '[PlainText, OctetStream, JSON] NoContent

server ::
  ( MonadTime     m
  , MonadLogger   m
  , MonadThrow    m
  , User.Modifier m
  )
  => Authorization.Session
  -> ServerT API m
server Authorization.Session {} pk = do
  let userID = undefined -- FIXME
-- server Authorization {about = Entity userID _} pk = do
  now <- currentTime
  Web.Error.ensureM $ User.updatePublicKey userID pk now
  return NoContent
