module Fission.CLI.User (ensureNotLoggedIn) where

import qualified Crypto.PubKey.Ed25519                     as Ed25519
import qualified Data.Yaml                                 as YAML
import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.Error.Types
import           Web.DID.Types

import qualified Fission.Web.Auth.Token.Types              as Auth
import           Fission.Web.Client

import           Fission.CLI.Environment
import           Fission.CLI.WebNative.Mutation.Auth.Store as UCAN

ensureNotLoggedIn ::
  ( MonadIO          m
  , MonadTime        m
  , MonadWebClient   m
  , MonadLogger      m
  , UCAN.MonadStore  m
  , ServerDID        m
  , MonadWebAuth     m Auth.Token
  , MonadWebAuth     m Ed25519.SecretKey
  , MonadEnvironment m
  , MonadCleanup     m
  , m `Raises` ClientError
  , m `Raises` AlreadyExists DID
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  )
  => m ()
ensureNotLoggedIn = do
  attempt getRootUserProof >>= \case
    Left _ ->
      return ()

    Right proof ->
      attempt (sendAuthedRequest proof whoAmI) >>= \case
        Right _ -> raise $ AlreadyExists @DID
        Left  _ -> return ()
