-- | Helpers for running the Fission applications
module Fission.Internal.App (runApp, isDebugEnabled, setRioVerbose) where

import           Fission.Environment (getFlagWithDefault)
import           Fission.Prelude
import           System.Environment  (setEnv, unsetEnv)

-- | Run the given simple app.
--   Set `DEBUG` to `True` to enable verbose app logging
runApp :: RIO SimpleApp a -> IO a
runApp simpleApp = do
  isVerbose  <- isDebugEnabled
  setRioVerbose isVerbose
  runSimpleApp simpleApp

-- | Check if `DEBUG` is set to `True`
isDebugEnabled :: IO Bool
isDebugEnabled = getFlagWithDefault "DEBUG" False

-- | Sets the env variable `RIO_VERBOSE` to the given value
--   Note: unsetting `RIO_VERBOSE` is the only way for it to be interpretted as False
setRioVerbose :: Bool -> IO ()
setRioVerbose True  = setEnv "RIO_VERBOSE" "true"
setRioVerbose False = unsetEnv "RIO_VERBOSE"
