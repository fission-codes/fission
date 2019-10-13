module Fission.CLI.Error.Types (Error (..)) where

import RIO

import qualified Fission.CLI.Auth    as Auth
import           Fission.Error.Class
import qualified Fission.IPFS.Error  as IPFS

data Error
  = AuthErr Auth.CLIError
  | IPFSErr IPFS.Error
  deriving ( Exception
           , Eq
           , Show
           )

instance SuperError Auth.CLIError Error where
  toError authErr = AuthErr authErr

instance SuperError IPFS.Error Error where
  toError ipfsErr = IPFSErr ipfsErr

instance SuperError IPFS.Add Error where
  toError = toError . IPFS.AddErr
