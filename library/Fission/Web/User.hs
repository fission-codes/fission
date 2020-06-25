module Fission.Web.User
  ( API
  , Auth
  , RegisterRoute
  , VerifyRoute
  , VerifyEmailRoute
  , UpdatePublicKeyRoute
  , ResetRoute
  , WhoAmIRoute
  , server
  ) where

import           Servant

import           Fission.Prelude
import           Fission.IPFS.DNSLink.Class as DNSLink

import qualified Fission.User as User
import           Fission.Email

import qualified Fission.Challenge.Creator.Class  as Challenge
import qualified Fission.Challenge.Verifier.Class as Challenge

import qualified Fission.Web.User.Create             as Create
import qualified Fission.Web.User.Verify             as Verify
import qualified Fission.Web.User.VerifyEmail        as VerifyEmail
import qualified Fission.Web.User.Password.Reset     as Reset
import qualified Fission.Web.User.UpdatePublicKey    as UpdatePublicKey
import qualified Fission.Web.User.UpdateExchangeKeys as UpdateExchangeKeys
import qualified Fission.Web.User.UpdateData         as UpdateData
import qualified Fission.Web.User.WhoAmI             as WhoAmI

import qualified Fission.Web.Auth.Types as Auth


type API
  =   RegisterRoute
 :<|> Create.PasswordAPI
 :<|> WhoAmIRoute
 :<|> VerifyRoute
 :<|> VerifyEmailRoute
 :<|> UpdatePublicKeyRoute
 :<|> UpdateExchangeKeysRoute
 :<|> UpdateDataRoute
 :<|> ResetRoute

type Auth
  = Auth.HigherOrder

type RegisterRoute
  = Auth.RegisterDID
    :> Create.API

type WhoAmIRoute
  = "whoami"
    :> Auth
    :> WhoAmI.API

type VerifyRoute
  = "verify"
    :> Auth
    :> Verify.API

type VerifyEmailRoute
  = "email"
    :> "verify"
    :> VerifyEmail.API

type UpdatePublicKeyRoute
  = "did"
    :> Auth
    :> UpdatePublicKey.API

type UpdateExchangeKeysRoute
  = "exchange"
    :> "keys"
    :> Auth
    :> UpdateExchangeKeys.API

type UpdateDataRoute
  = "data"
    :> Auth
    :> UpdateData.API

type ResetRoute
  = "reset_password"
    :> Auth
    :> Reset.API

server ::
  ( MonadDNSLink       m
  , MonadLogger        m
  , MonadTime          m
  , MonadEmail         m
  , User.Modifier      m
  , User.Creator       m
  , Challenge.Creator  m
  , Challenge.Verifier m
  )
  => ServerT API m
server = Create.withDID
    :<|> Create.withPassword
    :<|> WhoAmI.server
    :<|> (\_ -> Verify.server)
    :<|> VerifyEmail.server
    :<|> UpdatePublicKey.server
    :<|> UpdateExchangeKeys.server
    :<|> UpdateData.server
    :<|> Reset.server
