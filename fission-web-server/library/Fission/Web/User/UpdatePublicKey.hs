module Fission.Web.User.UpdatePublicKey
  ( API
  , server
  ) where

import           Servant

import           Fission.Prelude
import           Fission.Authorization
 
import qualified Fission.Web.Error as Web.Error

import qualified Fission.Key  as Key
import qualified Fission.User as User

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
  => Authorization
  -> ServerT API m
server Authorization {about = Entity userID _} pk = do
  now <- currentTime
  Web.Error.ensureM $ User.updatePublicKey userID pk now
  return NoContent
