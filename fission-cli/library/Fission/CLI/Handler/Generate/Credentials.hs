-- | Credentials command
module Fission.CLI.Handler.Generate.Credentials (credentials) where

import qualified RIO.Text                                  as Text

import qualified Crypto.PubKey.Ed25519                     as Ed25519
import           Crypto.Random.Types


import           Fission.Prelude

import qualified Fission.Internal.UTF8                     as UTF8

import           Fission.CLI.Environment                   as Env

import qualified Fission.CLI.Display.Error                 as CLI.Error
import qualified Fission.CLI.Display.Success               as CLI.Success


-- | The command to generate key pairs and DIDs
credentials ::
  ( MonadIO          m
  , MonadTime        m
  , MonadLogger      m
  , MonadEnvironment m
  , MonadCleanup     m
  , Show    (ErrorCase m)
  , Display (ErrorCase m)
  , CheckErrors m
  )
  => m ()
credentials = do
  logDebug @Text "Testing the credentials command"