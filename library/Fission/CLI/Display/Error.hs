-- | File sync, IPFS-style
module Fission.CLI.Display.Error
  ( put
  , put'
  , putErrOr
  , notConnected
  ) where

import qualified System.Console.ANSI              as ANSI

import qualified Fission.CLI.Environment.Override as Env.Override
import qualified Fission.Internal.UTF8            as UTF8
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

-- | Display an error message to a user encouraging them to run `fission app register`
--   Error depends on if they have basic auth saved somewhere (ie if they are an existing user)
notConnected :: (MonadIO m, MonadLogger m, Exception err) => err ->  m ()
notConnected err =
  Env.Override.findBasicAuth >>= \case
    Nothing ->
      put err "Not logged in yet! Try running `fission user register`"

    Just _auth ->
      put err $ mconcat
        [ "Thanks for updating fission! The CLI now uses private key authentication.\n"
        , "Upgrade by running `fission user register`"
        ]
