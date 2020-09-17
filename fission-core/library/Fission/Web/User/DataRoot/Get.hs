module Fission.Web.User.DataRoot.Get
  ( API
  , server
  ) where

import           Fission.Prelude
import           Servant

import qualified Fission.Web.Error           as Web.Err

import           Fission.User.Username.Types
import           Fission.DataRoot as DataRoot
import           Network.IPFS.CID.Types


type API
  =  Summary "Get Data Root"
  :> Description "Retrieve a user's data root from DNS"
  :> Capture "username" Username
  :> Get '[JSON, PlainText] CID

server ::
  ( MonadLogger   m
  , MonadDataRoot m
  )
  => ServerT API m
server username = Web.Err.ensureM $ DataRoot.get username