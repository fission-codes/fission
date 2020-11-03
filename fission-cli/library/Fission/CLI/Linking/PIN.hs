module Fission.CLI.Linking.PIN
  ( create
  , module Fission.CLI.Linking.PIN.Types
  ) where

import           Crypto.Random.Types

import qualified RIO.ByteString                as BS
import qualified RIO.Text                      as Text

import           Fission.Prelude

import           Fission.CLI.Linking.PIN.Types

create :: (MonadIO m, MonadLogger m) => m PIN
create = do
  randomBS <- liftIO $ getRandomBytes 6 -- NOTE we want actual IO for system enrtopy

  let
    txt :: Text
    txt = Text.takeEnd 6 . Text.pack . mconcat $ show <$> BS.unpack randomBS

  logDebug $ "Generated random Challenge: " <> txt
  return $ PIN txt
