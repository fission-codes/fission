module Fission.Web.User.UpdatePublicKey
  ( API
  , server
  ) where

import           Servant

import           Fission.Prelude
import           Fission.Authorization

import qualified Fission.Key  as Key
import qualified Fission.User as User

type API
  =  Summary "Update Public Key"
  :> Description "Set currently authenticated user's root public key to another one"
  :> ReqBody '[JSON] (Key.Public, Key.Algorithm)
  :> Patch   '[PlainText, OctetStream, JSON] NoContent

server ::
  ( MonadTime       m
  , MonadDB       t m
  , User.Modifier t
  )
  => Authorization
  -> ServerT API m
server Authorization {about = Entity userID _} (pk, alg) = do
  runDBNow $ User.updatePublicKey userID pk alg
  return NoContent
