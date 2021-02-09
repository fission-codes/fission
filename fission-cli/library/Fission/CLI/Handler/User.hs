module Fission.CLI.Handler.User
  ( interpret
  , Errs
  ) where

import           Data.Type.List
import qualified Data.Yaml                                      as YAML

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519                          as Ed25519
import qualified Crypto.PubKey.RSA.Types                        as RSA

import           Network.DNS
import qualified Network.IPFS.Process.Error                     as IPFS.Process
import qualified Network.IPFS.Types                             as IPFS

import           Servant.Client

import           Fission.Prelude

import           Fission.Error.Types
-- import qualified Fission.JSON.Error                             as JSON

import qualified Fission.Key                                    as Key
-- import           Fission.Key.IV.Error                           as IV

import           Fission.User.DID.Types
import qualified Fission.User.Username.Error                    as Username

import qualified Fission.IPFS.Error.Types                       as IPFS

import qualified Fission.Web.Auth.Token.JWT.Error               as JWT
-- import qualified Fission.Web.Auth.Token.JWT.Proof.Error         as JWT.Proof
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error      as UCAN.Resolver
import           Fission.Web.Auth.Token.JWT.Types

import qualified Fission.CLI.Base.Types                         as Base
import           Fission.CLI.Error.Types
-- import qualified Fission.CLI.Key.Store                          as Key.Store
import           Fission.CLI.Types

import qualified Fission.CLI.Handler                            as Handler
import qualified Fission.CLI.Handler.User.Login                 as Login

import qualified Fission.CLI.Parser.Command.User.Login.Types    as Login
import qualified Fission.CLI.Parser.Command.User.Register.Types as Register
import           Fission.CLI.Parser.Command.User.Types          as User

type Errs
  = '[ NotFound JWT
     , JWT.Error
     , UCAN.Resolver.Error
     , NotFound DID

     , ClientError
     , DNSError
     , NotFound DID
     , AlreadyExists Ed25519.SecretKey

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
     , IPFS.Process.Error
     ] ++ Login.Errs

interpret :: forall errs .
  ( Contains Errs errs
  , Contains errs errs
  , Display   (OpenUnion errs)
  , Exception (OpenUnion errs)
  )
  => User.Options
  -> FissionCLI errs Base.Config ()
interpret cmd = do
  logDebug @Text "App interpreter"

  case cmd of
    Login Login.Options {..} ->
      Handler.login username

    Register Register.Options {..} -> do
      Handler.register maybeUsername maybeEmail
      return ()

    WhoAmI _ ->
      Handler.whoami
