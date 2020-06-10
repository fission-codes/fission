module Fission.CLI.Environment.Types (Environment (..)) where

import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude

import           Fission.URL.Types
import           Fission.User.DID.Types

import           Fission.Internal.Orphanage.Glob.Pattern ()

-- | Virtual environment built up from many layers of 'Environment.Override'.
data Environment = Environment
  { peers     :: ![IPFS.Peer]
  , ignored   :: !IPFS.Ignored
  , appURL    :: !(Maybe URL)
  , buildDir  :: !(Maybe FilePath)
  , serverDID :: !(Maybe DID)
  }

instance ToJSON Environment where
  toJSON Environment {..} = object
    [ "peers"      .= peers
    , "app_url"    .= appURL
    , "ignore"     .= ignored
    , "build_dir"  .= buildDir
    , "server_did" .= serverDID
    ]

instance FromJSON Environment where
  parseJSON = withObject "Environment" \obj -> do
    peers     <- obj .:  "peers"
    ignored   <- obj .:  "ignored"

    serverDID <- obj .:? "server_did"
    appURL    <- obj .:? "app_url"
    buildDir  <- obj .:? "build_dir"

    return Environment {..}
