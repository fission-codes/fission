module Fission.Web.User.UpdatePublicKey
  ( API
  , server
  ) where

import           Servant

import           Fission.Prelude
import           Fission.Models

import           Fission.PublicKey.Types
import qualified Fission.User as User

type API
  =  Summary "Update Public Key"
  :> Description "Set currently authenticated user's root public key to another one"
  :> ReqBody '[JSON] (PublicKey, Algorithm)
  :> Patch   '[PlainText, OctetStream, JSON] NoContent

server ::
  ( MonadTime       m
  , MonadDB       t m
  , User.Modifier t
  )
  => Entity User
  -> ServerT API m
server (Entity userID _) (pk, alg) = do
  runDBNow $ User.updatePublicKey userID pk alg
  return NoContent
