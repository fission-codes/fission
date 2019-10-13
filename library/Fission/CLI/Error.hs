module Fission.CLI.Error where

import RIO

import qualified Fission.CLI.Auth   as Auth
import qualified Fission.IPFS.Error as IPFS

data Error
  = AuthErr Auth.CLIError
  | IPFSErr IPFS.Error
  deriving ( Exception
           , Eq
           , Show
           )

class ToError err where
  toError :: err -> Error

instance ToError Auth.CLIError where
  toError authErr = AuthErr authErr

instance ToError IPFS.Error where
  toError ipfsErr = IPFSErr ipfsErr

instance ToError IPFS.Add where
  toError = toError . IPFS.AddErr

eitherCLI :: ToError err => Either err b -> Either Error b
eitherCLI = either (Left . toError) Right
