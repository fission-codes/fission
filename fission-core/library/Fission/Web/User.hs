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

import           Fission.IPFS.DNSLink.Class          as DNSLink
import           Fission.Prelude

import           Fission.Email
import qualified Fission.User                        as User

import qualified Fission.Challenge.Creator.Class     as Challenge
import qualified Fission.Challenge.Verifier.Class    as Challenge
import           Fission.WNFS

import qualified Fission.Web.User.Create             as Create
import qualified Fission.Web.User.DataRoot           as DataRoot
import qualified Fission.Web.User.Password.Reset     as Reset
import qualified Fission.Web.User.UpdateExchangeKeys as UpdateExchangeKeys
import qualified Fission.Web.User.UpdatePublicKey    as UpdatePublicKey
import qualified Fission.Web.User.Verify             as Verify
import qualified Fission.Web.User.VerifyEmail        as VerifyEmail
import qualified Fission.Web.User.WhoAmI             as WhoAmI

import qualified Fission.Web.Auth.Types              as Auth


type API
  =   RegisterRoute
 :<|> Create.PasswordAPI
 :<|> WhoAmIRoute
 :<|> VerifyRoute
 :<|> VerifyEmailRoute
 :<|> UpdatePublicKeyRoute
 :<|> UpdateExchangeKeysRoute
 :<|> DataRootRoute
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

type DataRootRoute
  = "data"
    :> DataRoot.API

type ResetRoute
  = "reset_password"
    :> Auth
    :> Reset.API

server ::
  ( MonadDNSLink       m
  , MonadLogger        m
  , MonadTime          m
  , MonadEmail         m
  , MonadWNFS          m
  , MonadSTM           m
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
    :<|> DataRoot.server
    :<|> Reset.server
