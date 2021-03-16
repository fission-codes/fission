-- | Top level errors (that may escape the handler)
module Fission.CLI.Handler.Error.Types (Errs) where

import qualified Data.Yaml                                 as YAML

import           Crypto.Cipher.AES                         (AES256)
import           Crypto.Error
import qualified Crypto.PubKey.Ed25519                     as Ed25519
import qualified Crypto.PubKey.RSA.Types                   as RSA

import           Network.DNS                               as DNS
import qualified Network.IPFS.Add.Error                    as IPFS.Add
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Process.Error                as IPFS.Process
import qualified Network.IPFS.Types                        as IPFS

import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Error
import           Fission.IPFS.Error.Types                  as IPFS
import qualified Fission.JSON                              as JSON
import           Fission.URL.Types
import           Fission.User.DID.Types
import qualified Fission.User.Username.Error               as Username

import qualified Fission.Key.Error                         as Key
import qualified Fission.Key.IV.Error                      as IV
import qualified Fission.Key.Symmetric                     as Symmetric

import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
import qualified Fission.Web.Auth.Token.JWT.Proof.Error    as JWT.Proof
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver
import           Fission.Web.Auth.Token.JWT.Types

import qualified Fission.Web.Serialization                 as Web.Serialization

import           Fission.CLI.Environment                   as Env
import qualified Fission.CLI.Environment.OS                as OS
import           Fission.CLI.Error.Types
import           Fission.CLI.Linking.Status.Denied.Types
import           Fission.CLI.PIN.Types
import           Fission.CLI.PubSub.Secure.Payload         as SecurePayload

type Errs
  = '[ SomeException
     --
     , AlreadyExists DID
     , AlreadyExists Ed25519.SecretKey
     , AlreadyExists Env
     , AlreadyExists URL
     --
     , ClientError
     , CryptoError
     , DNSError
     --
     , Denied
     --
     , IPFS.Add.Error
     , IPFS.Process.Error
     , IPFS.UnableToConnect
     --
     , IV.GenError
     , JSON.Error
     --
     , JWT.Error
     , JWT.Proof.Error
     --
     , Key.Error
     , Mismatch PIN
     , NoKeyFile
     --
     , NotFound (Symmetric.Key AES256)
     , NotFound CID
     , NotFound DID
     , NotFound Ed25519.SecretKey
     , NotFound FilePath
     , NotFound JWT
     , NotFound URL
     , NotFound [IPFS.Peer]
     , NotRegistered
     --
     , OS.Unsupported
     , RSA.Error
     --
     , SecurePayload.Error
     , UCAN.Resolver.Error
     --
     , Username.Invalid
     , Web.Serialization.Error
     , YAML.ParseException
     ]
