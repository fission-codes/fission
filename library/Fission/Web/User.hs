module Fission.Web.User
  ( API
  , Auth
  , RegisterRoute
  , VerifyRoute
  , UpdateDIDRoute
  , ResetRoute
  , server
  ) where

import           Servant

import           Fission.Prelude
import           Fission.IPFS.DNSLink.Class as DNSLink

import qualified Fission.User as User

import qualified Fission.Web.User.Create          as Create
import qualified Fission.Web.User.Verify          as Verify
import qualified Fission.Web.User.Password.Reset  as Reset
import qualified Fission.Web.User.UpdateDID       as UpdateDID
import qualified Fission.Web.User.UpdateData      as UpdateData

import qualified Fission.Web.Auth.Types           as Auth

type API = RegisterRoute
      :<|> VerifyRoute
      :<|> UpdateDIDRoute
      :<|> UpdateDataRoute
      :<|> ResetRoute

type Auth
  = Auth.HigherOrder

type RegisterRoute
  = Auth.RegisterDid
    :> Create.API

type VerifyRoute
  = "verify"
    :> Auth
    :> Verify.API

type UpdateDIDRoute
  = "did"
    :> Auth
    :> UpdateDID.API

type UpdateDataRoute
  = "data"
    :> Auth
    :> UpdateData.API

type ResetRoute
  = "reset_password"
    :> Auth
    :> Reset.API

server ::
  ( MonadDNSLink     m
  , MonadLogger      m
  , MonadTime        m
  , MonadDB        t m
  , User.Creator   t
  , User.Modifier  t
  )
  => ServerT API m
server = Create.server
    :<|> Verify.server
    :<|> UpdateDID.server
    :<|> UpdateData.server
    :<|> Reset.server
