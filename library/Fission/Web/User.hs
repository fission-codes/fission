module Fission.Web.User
  ( server
  , API
  , VerifyRoute
  ) where

import           Servant

import           Fission.Prelude
import           Fission.IPFS.DNSLink.Class as DNSLink

import qualified Fission.User as User

import qualified Fission.Web.User.Create as Create
import qualified Fission.Web.User.Verify as Verify
import qualified Fission.Web.User.Password.Reset as Reset
import qualified Fission.Web.Auth.Types as Auth

type API = Create.API
      :<|> VerifyRoute
      :<|> ResetRoute

type VerifyRoute = "verify"
                   :> Auth.ExistingUser
                   :> Verify.API

type ResetRoute = "reset_password"
                  :> Auth.ExistingUser
                  :> Reset.API

server ::
  ( MonadDNSLink    m
  , MonadLogger     m
  , MonadTime       m
  , MonadDB       t m
  , User.Creator  t
  , User.Modifier t
  )
  => ServerT API m
server = Create.server
    :<|> (\_ -> Verify.server)
    :<|> Reset.server
