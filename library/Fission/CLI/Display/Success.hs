-- | Success messages and conditions
module Fission.CLI.Display.Success
  ( live
  , putOk
  , dnsUpdated
  ) where

import RIO

import qualified System.Console.ANSI as ANSI

import qualified Fission.Internal.UTF8 as UTF8

live :: MonadIO m => Text -> m ()
live hash = do
  UTF8.putText $ "🚀 Now live on the network\n"
  UTF8.putText $ "👌 " <> hash  <> "\n"

putOk :: MonadIO m => Text -> m ()
putOk msg = do
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  UTF8.putText $ "✅ " <> msg <> "\n"
  liftIO $ ANSI.setSGR [ANSI.Reset]

dnsUpdated :: MonadIO m => Text -> m ()
dnsUpdated domain = do
  UTF8.putText "📝 DNS updated! Check out your site at: \n"
  UTF8.putText $ "🔗 " <> domain  <> "\n"
