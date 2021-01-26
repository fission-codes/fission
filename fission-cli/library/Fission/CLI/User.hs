module Fission.CLI.User (ensureNotLoggedIn) where

import qualified Crypto.PubKey.Ed25519           as Ed25519
import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.Error.Types
import           Fission.User.DID.Types

import qualified Fission.Web.Auth.Token.Types    as Auth
import           Fission.Web.Client
import qualified Fission.Web.Client.User         as User

import           Fission.CLI.Display.Success     as CLI.Success

ensureNotLoggedIn ::
  ( MonadIO        m
  , MonadTime      m
  , MonadWebClient m
  , ServerDID      m
  , MonadWebAuth   m Auth.Token
  , MonadWebAuth   m Ed25519.SecretKey
  , MonadCleanup   m
  , m `Raises` ClientError
  , m `Raises` AlreadyExists DID
  )
  => m ()
ensureNotLoggedIn =
  attempt (sendAuthedRequest User.whoami) >>= \case
    Right username -> do
      CLI.Success.alreadyLoggedInAs username
      raise $ AlreadyExists @DID

    Left _ ->
      return ()
