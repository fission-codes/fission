module Fission.CLI.Environment.Override.Types (Override (..)) where

import           RIO.Prelude.Types
import           Servant.API

import qualified Network.IPFS.Types as IPFS
 
import           Fission.Prelude
import           Fission.Internal.Orphanage.BasicAuthData ()
import           Fission.Internal.Orphanage.Glob.Pattern ()

data Override = Override
  { peers         :: [IPFS.Peer]
  , maybeUserAuth :: Maybe BasicAuthData
  , maybeIgnored  :: Maybe IPFS.Ignored
  , maybeBuildDir :: Maybe FilePath
  }

instance Semigroup Override where
  a <> b = Override
    { peers         = peers         a <>  peers         b
    , maybeUserAuth = maybeUserAuth a <|> maybeUserAuth b
    , maybeIgnored  = maybeIgnored  a <|> maybeIgnored  b
    , maybeBuildDir = maybeBuildDir a <|> maybeBuildDir b
    }

instance Monoid Override where
  mempty = Override
    { peers         = []
    , maybeUserAuth = Nothing
    , maybeIgnored  = Nothing
    , maybeBuildDir = Nothing
    }

instance ToJSON Override where
  toJSON Override {..} = object
    [ "peers"     .= peers
    , "user_auth" .= maybeUserAuth
    , "ignore"    .= maybeIgnored
    , "build_dir" .= maybeBuildDir
    ]

instance FromJSON Override where
  parseJSON = withObject "Override" \obj -> do
    peers         <- obj .:? "peers" .!= []
    maybeUserAuth <- obj .:? "user_auth"
    maybeIgnored  <- obj .:? "ignore"
    maybeBuildDir <- obj .:? "build_dir"
    return Override {..}
