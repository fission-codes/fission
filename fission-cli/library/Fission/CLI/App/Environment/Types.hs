module Fission.CLI.App.Environment.Types (Env (..)) where

import           Fission.Prelude

import           Fission.URL

-- | This is the part that actually gets written to disk.
--   'Environment' is constructed virtually from layers of 'Env's.
data Env = Env
  { ipfsIgnored :: ![Text]
  , appURL      :: !URL
  , buildDir    :: !FilePath
  }

instance ToJSON Env where
  toJSON Env {..} = object
    [ "ignore" .= ipfsIgnored
    , "url"    .= appURL
    , "build"  .= buildDir
    ]

instance FromJSON Env where
  parseJSON = withObject "Env" \obj -> do
    ipfsIgnored <- obj .:? "ignore" .!= []
    buildDir    <- obj .:? "build"  .!= "."
    appURL      <- obj .:  "url"

    return Env {..}
