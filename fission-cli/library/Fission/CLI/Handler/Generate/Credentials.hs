-- | Credentials command
module Fission.CLI.Handler.Generate.Credentials (credentials) where

import qualified Crypto.PubKey.Ed25519                     as Ed25519
import           Crypto.Random.Types

import qualified System.Console.ANSI                       as ANSI


import           Fission.Prelude

import qualified Fission.Key                               as Key
import qualified Fission.Internal.UTF8                     as UTF8

import           Fission.CLI.Display.Text

import qualified Fission.CLI.Display.Success               as CLI.Success

import           Web.DID.Types                             as DID
import qualified Web.UCAN.Internal.Base64                  as B64

-- | The command to generate key pairs and DIDs
credentials ::
  ( MonadIO          m
  , MonadRandom m
  , MonadLogger      m
  , MonadCleanup     m
  )
  => m ()
credentials = do
  logDebug @Text "🪄🗝️ Generating a key pair..."

  secretKey <- Ed25519.generateSecretKey

  let 
    publicKey = Ed25519.toPublic secretKey
    did = DID.Key $ Key.Ed25519PublicKey publicKey

  CLI.Success.putOk "Generated an Ed25519 key pair and associated DID"

  UTF8.putText "🗝️  Private key: "
  colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] do
    UTF8.putTextLn $ decodeUtf8Lenient (B64.toB64ByteString secretKey)

  UTF8.putText "🔑 Public key: "
  colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] do
    UTF8.putTextLn $ textDisplay publicKey

  UTF8.putText "🆔 DID: "
  colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] do
    UTF8.putTextLn $ textDisplay did