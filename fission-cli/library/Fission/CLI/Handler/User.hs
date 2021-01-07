module Fission.CLI.Handler.User
  ( interpret
  , Errs
  ) where

import           Data.Type.List
import qualified Data.Yaml                                      as YAML

import           Crypto.Cipher.AES                              (AES256)
import           Crypto.Error                                   as Crypto
import qualified Crypto.PubKey.Ed25519                          as Ed25519
import qualified Crypto.PubKey.RSA.Types                        as RSA

import           Network.DNS
import qualified Network.IPFS.Add.Error                         as IPFS.Add
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Process.Error                     as IPFS.Process
import qualified Network.IPFS.Types                             as IPFS

import           Servant.Client

import           Fission.Prelude

import           Fission.Error.Types
import qualified Fission.Key                                    as Key
import           Fission.Key.IV.Error                           as IV
import qualified Fission.Key.Symmetric                          as Symmetric

import           Fission.User.DID.Types
import qualified Fission.User.Username.Error                    as Username

import qualified Fission.IPFS.Error.Types                       as IPFS

import qualified Fission.Web.Auth.Token.JWT.Error               as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error      as UCAN.Resolver
import           Fission.Web.Auth.Token.JWT.Types
import qualified Fission.Web.Serialization                      as Web.Serialization

import qualified Fission.CLI.Base.Types                         as Base
import           Fission.CLI.Error.Types
import           Fission.CLI.Linking.Status
import           Fission.CLI.PIN                                as PIN
import           Fission.CLI.Types

import qualified Fission.CLI.Handler                            as Handler
import qualified Fission.CLI.Handler.User.Login                 as Login

import qualified Fission.CLI.Parser.Command.User.Register.Types as Register
import           Fission.CLI.Parser.Command.User.Types          as User

type Errs
  = '[ NotFound JWT
     , NotFound (Symmetric.Key AES256)
     , JWT.Error
     , UCAN.Resolver.Error
     , NotFound DID

     , ClientError
     , DNSError
     , NotFound CID
     , NotFound DID
     , AlreadyExists Ed25519.SecretKey

     , YAML.ParseException

     , Username.Invalid
     , CryptoError

     , CryptoError
     , IV.GenError

     , NoKeyFile
     , Key.Error
     , NotFound Ed25519.SecretKey
     , NotFound FilePath
     , NotFound [IPFS.Peer]
     , NotRegistered
     , SomeException
     , RSA.Error
     , CryptoError
     , Web.Serialization.Error
     , Mismatch PIN
     , Denied

     , IPFS.UnableToConnect
     , IPFS.Process.Error
     , IPFS.Add.Error
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
    Login _ -> do
      Handler.login
      return ()

    Register Register.Options {..} -> do
      Handler.register maybeUsername maybeEmail
      return ()

    WhoAmI _ ->
      Handler.whoami
