module Fission.Web.Client.User
  ( Register
  , Verify
  , WhoAmI
  , UpdatePK
  , ExchangeKeysAPI
  , addExchangePK
  ) where

import           Servant.API

import           Fission.Web.Routes              (UserPrefix)
import qualified Fission.Web.User                as User


import qualified Crypto.PubKey.Ed25519           as Ed25519
import qualified Crypto.PubKey.RSA               as RSA

import           Servant.Client

import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.User.DID.Types

import           Fission.Web.Auth.Token
import           Fission.Web.Client              as Client

type Register = UserPrefix :> User.RegisterRoute
type Verify   = UserPrefix :> User.VerifyRoute
type WhoAmI   = UserPrefix :> User.WhoAmIRoute
type UpdatePK = UserPrefix :> User.UpdatePublicKeyRoute

type ExchangeKeysAPI = UserPrefix :> User.UpdateExchangeKeysRoute

addExchangePK ::
  ( MonadIO      m
  , MonadTime    m
  , MonadLogger  m
  , ServerDID    m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => m (RSA.PublicKey -> ClientM [RSA.PublicKey])
addExchangePK = do
  (addExchangeKey :<|> _) <- authClient $ Proxy @ExchangeKeysAPI
  return addExchangeKey
