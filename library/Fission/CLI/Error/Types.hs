module Fission.CLI.Error.Types (Error (..)) where

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
