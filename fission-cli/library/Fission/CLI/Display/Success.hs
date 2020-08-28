-- | Success messages and conditions
module Fission.CLI.Display.Success
  ( live
  , putOk
  , dnsUpdated
  , currentlyLoggedInAs
  , alreadyLoggedInAs
  ) where

import qualified System.Console.ANSI      as ANSI

import           Fission.Prelude

import qualified Fission.Internal.UTF8    as UTF8
import           Fission.URL

import           Fission.CLI.Display.Text

live :: MonadIO m => m ()
live = UTF8.putTextLn $ "🚀 Now live on the network"

putOk :: (MonadIO m, MonadCleanup m) => Text -> m ()
putOk msg =
  colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green] do
    UTF8.putText $ "✅ " <> msg <> "\n"

dnsUpdated :: MonadIO m => URL -> m ()
dnsUpdated domain = do
  UTF8.putTextLn "📝 DNS updated! Check out your site at: "
  UTF8.putTextLn $ "🔗 " <> textDisplay domain

currentlyLoggedInAs :: (MonadIO m, MonadCleanup m) => Text -> m ()
currentlyLoggedInAs = loggedInAs "Currently logged in as: "

alreadyLoggedInAs :: (MonadIO m, MonadCleanup m) => Text -> m ()
alreadyLoggedInAs = loggedInAs "Already logged in as: "

loggedInAs :: (MonadIO m, MonadCleanup m) => Text -> Text -> m ()
loggedInAs msg username = do
  UTF8.putText $ "💻 " <> msg

  colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] do
    UTF8.putTextLn username
