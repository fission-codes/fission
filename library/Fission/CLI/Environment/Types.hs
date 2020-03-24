module Fission.CLI.Environment.Types (Environment (..)) where

import Fission.Prelude

import qualified Network.IPFS.Types as IPFS
import           Fission.Internal.Orphanage.Glob.Pattern ()

data Environment = Environment
  { peers    :: Maybe (NonEmpty IPFS.Peer)
  , ignored  :: IPFS.Ignored
  , buildDir :: Maybe (FilePath)
  }

instance ToJSON Environment where
  toJSON Environment {..} = object
    [ "peers"     .= peers
    , "ignore"    .= ignored
    , "build_dir" .= buildDir
    ]

instance FromJSON Environment where
  parseJSON = withObject "Environment" <| \obj ->
    Environment <$> obj .: "peers"
                <*> obj .: "ignore"
                <*> obj .: "build_dir"
