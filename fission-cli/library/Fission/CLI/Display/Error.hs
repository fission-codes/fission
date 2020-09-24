-- | File sync, IPFS-style
module Fission.CLI.Display.Error
  ( put
  , put'
  , putErrOr
  ) where

import qualified System.Console.ANSI   as ANSI

import qualified Fission.Internal.UTF8 as UTF8
import           Fission.Prelude

-- | Display a given error to the user and log an error to the debug log.
put :: (MonadIO m, MonadLogger m, Show err) => err -> Text -> m ()
put err msg = do
  logDebug $ displayShow err
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  UTF8.putText $ "🚫 " <> msg <> "\n"
  liftIO $ ANSI.setSGR [ANSI.Reset]

-- | Display a generic error message to the user and log an error to the debug log.
put' :: (MonadIO m, MonadLogger m, Show err) => err -> m ()
put' err = put err $ mconcat
  [ "Something went wrong. Please try again or contact "
  , "Fission live chat support at https://fission.codes/support"
  ]

putErrOr :: (MonadIO m, MonadLogger m, Show err) => (t -> m ()) -> Either err t -> m ()
putErrOr cont = \case
  Left err  -> put' err
  Right val -> cont val
