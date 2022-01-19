module Fission.CLI.Environment.Types (Env (..)) where

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Types          as IPFS

import           Fission.Prelude

import           Fission.Error.Types

import           Fission.User.Username.Types
import           Web.DID.Types

-- | "Global" environment
data Env = Env
  -- IPFS
  { peers          :: [IPFS.Peer]
  , ignored        :: [Text] -- ^ Passing through verbatim for ipfsignore

  -- Server
  , serverDID      :: DID

  -- Account
  , signingKeyPath :: FilePath
  , username       :: Username
  , rootProof      :: Maybe CID

  -- Releases
  , updateChecked  :: UTCTime
  }
  deriving (Eq, Show)

instance ToJSON Env where
  toJSON Env {..} = object
    [ "peers"            .= peers
    , "ignore"           .= ignored
    , "server_did"       .= serverDID
    , "signing_key_path" .= signingKeyPath
    , "username"         .= username
    , "root_proof"       .= rootProof
    , "update_checked"   .= updateChecked
    ]

instance FromJSON Env where
  parseJSON = withObject "Env" \obj -> do
    peers          <- obj .:  "peers"
    ignored        <- obj .:? "ignore" .!= []
    serverDID      <- obj .:  "server_did"
    signingKeyPath <- obj .:  "signing_key_path"
    username       <- obj .:  "username"
    rootProof      <- obj .:? "root_proof"
    updateChecked  <- obj .:? "update_checked" .!= fromSeconds 0

    return Env {..}

instance Display (AlreadyExists Env) where
  display _ = "Fission CLI config already exists"
