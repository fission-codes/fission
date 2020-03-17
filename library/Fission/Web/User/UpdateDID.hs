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

type API = ReqBody '[JSON] DID
        :> Post    '[JSON] DID

server ::
  ( MonadTime       m
  , MonadDB       t m
  , User.Modifier t
  )
  => Entity User
  -> ServerT API m
server (Entity userID _) did =
  did
    |> User.updateDID userID
    |> runDBNow
