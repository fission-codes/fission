module Fission.Web.User.UpdatePublicKey
  ( API
  , server
  ) where

import           Fission.Prelude
import           Fission.Models

import           Servant
import           Database.Esqueleto

import qualified Fission.User as User

import Fission.User.DID.Types
-- import           Fission.PublicKey.Types

type API
  =  Summary "Update Public Key"
  :> Description "Set currently authenticated user's root public key to another one"
  -- :> ReqBody '[JSON] DID -- FIXME also need the algo?
  :> ReqBody '[JSON] PublicKey -- FIXME also need the algo?
  :> Patch   '[PlainText, OctetStream, JSON] NoContent

server ::
  ( MonadTime       m
  , MonadDB       t m
  , User.Modifier t
  )
  => Entity User
  -> ServerT API m
-- server (Entity userID _) DID { publicKey, algorithm } = do
server (Entity userID _) pk = do
  runDBNow $ User.updatePublicKey userID pk Ed25519
  return NoContent
