module Fission.CLI.Error where

import RIO

import Fission.Internal.Constraint

import           Fission.CLI.Error.Types
import qualified Fission.CLI.Display.Error as CLI.Error

cliLog :: (MonadRIO cfg m, HasLogFunc cfg) => Error -> m ()
cliLog = CLI.Error.put'

-- eitherCLI :: ToError err => Either err b -> Either Error b
-- eitherCLI = either (Left . toError) Right

-- import qualified Fission.CLI.Auth   as Auth
-- import qualified Fission.IPFS.Error as IPFS

-- data Error
--   = AuthErr Auth.CLIError
--   | IPFSErr IPFS.Error
--   deriving ( Exception
--            , Eq
--            , Show
--            )

-- class ToError err where
--   toError :: err -> Error

-- instance ToError Auth.CLIError where
--   toError authErr = AuthErr authErr

-- instance ToError IPFS.Error where
--   toError ipfsErr = IPFSErr ipfsErr

-- instance ToError IPFS.Add where
--   toError = toError . IPFS.AddErr
