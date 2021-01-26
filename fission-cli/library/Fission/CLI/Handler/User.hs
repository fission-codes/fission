module Fission.CLI.Handler.User
  ( interpret
  , Errs
  ) where

import           Control.Monad.Base

import           Data.Type.List
import qualified Data.Yaml                                      as YAML

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519                          as Ed25519
import qualified Crypto.PubKey.RSA.Types                        as RSA

import           Network.DNS
import qualified Network.DNS                                    as DNS
import qualified Network.IPFS.Types                             as IPFS

import           Servant.Client
import           Servant.Client.Core

import           Fission.Prelude

import qualified Fission.CLI.Base.Types                         as Base
import           Fission.CLI.Types

import qualified Fission.CLI.Handler                            as Handler

import qualified Fission.CLI.Parser.Command.User.Login.Types    as Login
import           Fission.CLI.Parser.Command.User.Types          as User
import           Fission.Error.Types

import           Fission.User.DID.Types
import qualified Fission.Web.Auth.Token.JWT                     as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error               as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error      as UCAN.Resolver



import           Fission.CLI.Connected                          as Connected
import           Fission.Error
import qualified Fission.Key                                    as Key

import qualified Fission.Internal.UTF8                          as UTF8

import           Fission.Error
import qualified Fission.Key                                    as Key
import           Fission.URL.Types
import           Fission.User.DID.Types
import qualified Fission.User.Username.Error                    as Username

import qualified Fission.IPFS.Error.Types                       as IPFS

import           Fission.CLI.Types

import qualified Fission.CLI.Base.Types                         as Base
import           Fission.CLI.Connected                          as Connected
import           Fission.CLI.Error.Types

import           Fission.CLI.App.Environment                    as App.Env
import qualified Fission.CLI.Display.Error                      as CLI.Error
import qualified Fission.CLI.Handler                            as Handler

import           Fission.CLI.Parser.Command.App                 as App
import qualified Fission.CLI.Parser.Command.App.Info            as App.Info
import           Fission.CLI.Parser.Command.App.Init            as App.Init
import           Fission.CLI.Parser.Command.App.Up.Types        as App.Up
import qualified Fission.CLI.Parser.Config.IPFS                 as IPFS

import qualified Fission.CLI.Parser.Command.User.Register.Types as Register




  -- FIXME login
import qualified Fission.CLI.Key.Store                          as Key.Store
import qualified Fission.JSON.Error                             as JSON
import           Fission.Key.IV.Error                           as IV
import           Fission.Web.Auth.Token.JWT                     as JWT
import qualified Fission.Web.Auth.Token.JWT.Error               as JWT.Error
import qualified Fission.Web.Auth.Token.JWT.Proof.Error         as JWT.Proof
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class      as JWT

import qualified Fission.CLI.Handler.User.Login                 as Login


type Errs
  = '[ -- String
      JWT.Error
     , UCAN.Resolver.Error
     , NotFound DID
     -- , ActionNotAuthorized UCAN.JWT -- FIXME shoudl be more contextual

     , ClientError
     , DNSError
     , NotFound DID
     , AlreadyExists Ed25519.SecretKey
     -- , AlreadyExists DID

     , YAML.ParseException

     , Username.Invalid
     , CryptoError

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

     -- login

     , AlreadyExists DID
     , ClientError
     , CryptoError
     , IV.GenError
     , JSON.Error
     , JWT.Proof.Error
     , Key.Store.Error
     , NotFound DID
     , RSA.Error
     , UCAN.Resolver.Error
     ] ++ Login.Errs

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
    Login Login.Options {..} ->
      Handler.login username

    Register Register.Options {..} -> do
      Handler.register maybeUsername maybeEmail
      return ()

    WhoAmI _ ->
      Handler.whoami
