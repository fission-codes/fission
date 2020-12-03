module Fission.CLI.User
  ( interpret
  , Errs
  ) where

import           Fission.Prelude

import qualified Fission.CLI.Base.Types                      as Base
import           Fission.CLI.Types

import qualified Fission.CLI.Handler                         as Handler

import qualified Fission.CLI.Parser.Command.User.Login.Types as Login
import           Fission.CLI.Parser.Command.User.Types       as User
import           Fission.Error.Types

import           Fission.User.DID.Types
import qualified Fission.Web.Auth.Token.JWT                  as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error            as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error   as UCAN.Resolver

import qualified Crypto.PubKey.Ed25519                       as Ed25519

import           Network.DNS

import           Servant.Client

import qualified Data.Yaml                                   as YAML

import           Control.Monad.Base

import           Fission.CLI.Connected                       as Connected
import           Fission.Error
import qualified Fission.Key                                 as Key


import qualified Crypto.PubKey.Ed25519                       as Ed25519
import qualified Data.Yaml                                   as YAML
import           Servant.Client.Core

import qualified Network.DNS                                 as DNS
import qualified Network.IPFS.Types                          as IPFS

import           Fission.Prelude

import qualified Fission.Internal.UTF8                       as UTF8

import           Fission.CLI.Types
import           Fission.Error
import qualified Fission.Key                                 as Key
import           Fission.Models
import           Fission.User.DID.Types

import qualified Fission.IPFS.Error.Types                    as IPFS
import           Fission.URL.Types

import qualified Fission.CLI.Base.Types                      as Base
import           Fission.CLI.Connected                       as Connected
import           Fission.CLI.Error.Types

import           Fission.CLI.App.Environment                 as App.Env
import qualified Fission.CLI.Display.Error                   as CLI.Error
import qualified Fission.CLI.Handler                         as Handler

import           Fission.CLI.Parser.Command.App              as App
import qualified Fission.CLI.Parser.Command.App.Info         as App.Info
import           Fission.CLI.Parser.Command.App.Init         as App.Init
import           Fission.CLI.Parser.Command.App.Up.Types     as App.Up
import qualified Fission.CLI.Parser.Config.IPFS              as IPFS

import           Crypto.Error
import qualified Crypto.PubKey.RSA.Types                     as RSA

type Errs
  = '[ String
     , JWT.Error
     , UCAN.Resolver.Error
     , NotFound DID
     , ActionNotAuthorized UCAN.JWT -- FIXME shoudl be more contextual

     , ClientError
     , DNSError
     , NotFound DID
     , AlreadyExists Ed25519.SecretKey
     -- , AlreadyExists DID


     , YAML.ParseException


     , NoKeyFile
     , Key.Error
     , NotFound Ed25519.SecretKey
     , NotFound FilePath
     , NotFound [IPFS.Peer]
     , NotRegistered
     , SomeException
     , RSA.Error
     , CryptoError
     , IPFS.UnableToConnect
     ]

interpret :: forall errs .
  ( Contains Errs errs
  , Contains errs errs
  , Display   (OpenUnion errs)
  , Exception (OpenUnion errs)
  )
  => Base.Config
  -> User.Options
  -> FissionCLI errs Base.Config ()
interpret baseCfg cmd = do
  logDebug @Text "App interpreter"

  case cmd of
    Register _ ->
      undefined -- FIXME void Handler.register

    WhoAmI _ ->
      undefined -- FIXME  Handler.whoami

    Login Login.Options {..} ->
      void . run' $ Handler.login username

  where
    run' ::
         FissionCLI errs Connected.Config a
      -> FissionCLI errs Base.Config (Either (OpenUnion errs) a)
    run' = Connected.run baseCfg 3000 -- FIXME timeoutSeconds

--     Info (App.Info.Options _) ->
--       Handler.appInfo
--
--     Init App.Init.Options {appDir, buildDir, maySubdomain, ipfsCfg = IPFS.Config {..}} -> do
--       let
--         run' ::
--              FissionCLI errs Connected.Config a
--           -> FissionCLI errs Base.Config (Either (OpenUnion errs) a)
--         run' = Connected.run baseCfg timeoutSeconds
--
--       attempt App.Env.read >>= \case
--         Right Env {appURL} -> do
--           UTF8.putTextLn $ "App already set up at " <> textDisplay appURL
--           logDebug . textDisplay $ AlreadyExists @App
--           raise $ AlreadyExists @App
--
--         Left errs -> do
--           case openUnionMatch errs of
--             Just (_ :: NotFound FilePath) -> do
--               logDebug @Text "Setting up new app"
--               _ <- run' $ Handler.appInit appDir buildDir maySubdomain
--               return ()
--
--             Nothing -> do
--               logError @Text "Problem setting up new app"
--               raise errs
--
--     Up App.Up.Options {watch, updateDNS, updateData, filePath, ipfsCfg = IPFS.Config {..}} -> do
--       let
--         run' :: MonadIO m => FissionCLI errs Connected.Config a -> m ()
--         run' = void . Connected.run baseCfg timeoutSeconds
--
--       attempt App.Env.read >>= \case
--         Right Env {appURL} ->
--           run' $ Handler.publish watch run' appURL filePath updateDNS updateData
--
--         Left _ ->
--           CLI.Error.put (NotFound @URL)
--             "You have not set up an app. Please run `fission app register`"
