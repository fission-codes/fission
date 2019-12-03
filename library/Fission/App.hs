-- | Helpers for running the Fission applications
module Fission.App (runApp) where

import System.Environment (setEnv, unsetEnv)
import Fission.Environment (getFlagWithDefault)
import Fission.Prelude

-- | Run the given simple app.
--   Set `DEBUG` to `True` to enable verbose app logging
runApp :: RIO SimpleApp a -> IO a
runApp simpleApp = do
  isVerbose  <- getFlagWithDefault "DEBUG" False
  setRioVerbose isVerbose
  runSimpleApp simpleApp

-- | Sets the env variable `RIO_VERBOSE` to the given value
--   Note: unsetting `RIO_VERBOSE` is the only way for it to be interpretted as False
setRioVerbose :: Bool -> IO ()
setRioVerbose True = setEnv "RIO_VERBOSE" "true"
setRioVerbose False = unsetEnv "RIO_VERBOSE"

