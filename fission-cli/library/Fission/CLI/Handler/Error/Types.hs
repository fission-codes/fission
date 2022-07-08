-- | Top level errors (that may escape the handler)
module Fission.CLI.Handler.Error.Types (Errs) where

import qualified Data.Yaml                                 as YAML

import           Crypto.Cipher.AES                         (AES256)
import           Crypto.Error
import qualified Crypto.PubKey.Ed25519                     as Ed25519
import qualified Crypto.PubKey.RSA.Types                   as RSA

import           Network.DNS                               as DNS
import qualified Network.IPFS.Add.Error                    as IPFS.Add
import qualified Network.IPFS.Files.Error              as IPFS.Files
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Process.Error                as IPFS.Process
import qualified Network.IPFS.Types                        as IPFS

import           Servant.Client.Core

import qualified Crypto.Key.Asymmetric.Algorithm.Error     as Asymmetric.Algorithm

import           Web.DID.Types
import qualified Web.UCAN.Error                            as UCAN
import qualified Web.UCAN.Proof.Error                      as UCAN.Proof
import qualified Web.UCAN.Resolver.Error                   as UCAN.Resolver


import           Fission.Prelude

import           Fission.Error
import           Fission.IPFS.Error.Types                  as IPFS
import qualified Fission.JSON                              as JSON
import           Fission.URL.Types
import qualified Fission.User.Username.Error               as Username

import qualified Fission.Key.Error                         as Key
import qualified Fission.Key.IV.Error                      as IV
import qualified Fission.Key.Symmetric                     as Symmetric

import           Fission.Web.Auth.Token.UCAN.Types
import           Fission.Web.Auth.Token.UCAN.Potency.Types
import qualified Fission.Web.Serialization                 as Web.Serialization

import           Fission.CLI.Environment                   as Env
import qualified Fission.CLI.Environment.OS                as OS
import           Fission.CLI.Error.Types
import           Fission.CLI.Linking.Status.Types
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
     , Status Denied
     --
     , IPFS.Add.Error
     , IPFS.Files.Error
     , IPFS.Process.Error
     , IPFS.UnableToConnect
     --
     , IV.GenError
     , JSON.Error
     --
     , UCAN.Error
     , UCAN.Proof.Error
     --
     , Key.Error
     , Mismatch PIN
     , NoKeyFile
     , Asymmetric.Algorithm.Invalid
     --
     , NotFound (Symmetric.Key AES256)
     , NotFound CID
     , NotFound DID
     , NotFound Ed25519.SecretKey
     , NotFound FilePath
     , NotFound UCAN
     , NotFound URL
     , NotFound [IPFS.Peer]
     , NotRegistered
     , NotSetup
     --
     , ParseError DID
     , ParseError Potency
     , ParseError UCAN
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
