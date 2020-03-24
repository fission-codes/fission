module Fission.Web.User.UpdateDID
  ( API
  , server
  ) where

import           Fission.Prelude
import           Fission.Models

import           Servant
import           Database.Esqueleto

import qualified Fission.User as User
import           Fission.User.DID.Types

type API
  =  Summary "Update DID"
  :> Description "Set currently authenticated user's root DID to another public key"
  :> ReqBody '[JSON] DID
  :> Patch   '[PlainText, OctetStream, JSON] NoContent

server ::
  ( MonadTime       m
  , MonadDB       t m
  , User.Modifier t
  )
  => Entity User
  -> ServerT API m
server (Entity userID _) did = do
  runDBNow (User.updateDID userID did)
  return NoContent
