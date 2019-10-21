-- | File sync, IPFS-style
module Fission.CLI.Display.Error (put, put') where

import           RIO

import qualified System.Console.ANSI as ANSI

import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8 as UTF8

-- | Display a given error to the user and log an error to the debug log.
put :: (MonadRIO cfg m, HasLogFunc cfg, Show err) => err -> Text -> m ()
put err msg = do
  logDebug $ displayShow err
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  UTF8.putText $ "🚫 " <> msg <> "\n"
  liftIO $ ANSI.setSGR [ANSI.Reset]

-- | Display a generic error message to the user and log an error to the debug log.
put' :: (MonadRIO cfg m, HasLogFunc cfg, Show err) => err -> m ()
put' err = put err $ mconcat
  [ "Something went wrong. Please try again or file a bug report with "
  , "Fission support at https://github.com/fission-suite/web-api/issues/new"
  ]
