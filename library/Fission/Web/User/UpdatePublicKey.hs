module Fission.Web.User.UpdatePublicKey
  ( API
  , server
  ) where

import           Fission.Prelude
import           Fission.Models

import           Servant
import           Database.Esqueleto

import           Fission.PublicKey.Types
import qualified Fission.User as User

type API
  =  Summary "Update Public Key"
  :> Description "Set currently authenticated user's root public key to another one"
  :> ReqBody '[JSON] PublicKey -- FIXME also need the algo
  :> Patch   '[PlainText, OctetStream, JSON] NoContent

server ::
  ( MonadTime       m
  , MonadDB       t m
  , User.Modifier t
  )
  => Entity User
  -> ServerT API m
server (Entity userID _) (Key { publicKey, algorithm }) = do
  runDBNow (User.updatePublicKey userID publicKey algorithm)
  return NoContent
