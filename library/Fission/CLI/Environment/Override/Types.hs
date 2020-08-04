module Fission.CLI.Environment.Override.Types (Override (..)) where

import           RIO.Prelude.Types

import           Servant.API

import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude
 
import           Fission.URL
import           Fission.User.DID

import           Fission.Internal.Orphanage.BasicAuthData ()
import           Fission.Internal.Orphanage.Glob.Pattern ()

-- | This is the part that actually gets written to disk.
--   'Environment' is constructed virtually from layers of 'Override's.
data Override = Override
  { peers          :: ![IPFS.Peer]
  , maybeAppURL    :: !(Maybe URL)
  , maybeUserAuth  :: !(Maybe BasicAuthData) -- TODO deprecated
  , maybeIgnored   :: !(Maybe IPFS.Ignored)
  , maybeBuildDir  :: !(Maybe FilePath)
  , maybeServerDID :: !(Maybe DID)
  }

instance Semigroup Override where
  a <> b = Override
    { peers          = peers          a <>  peers          b
    , maybeAppURL    = maybeAppURL    a <|> maybeAppURL    b
    , maybeUserAuth  = maybeUserAuth  a <|> maybeUserAuth  b
    , maybeIgnored   = maybeIgnored   a <|> maybeIgnored   b
    , maybeBuildDir  = maybeBuildDir  a <|> maybeBuildDir  b
    , maybeServerDID = maybeServerDID a <|> maybeServerDID b
    }

instance Monoid Override where
  mempty = Override
    { peers          = []
    , maybeAppURL    = Nothing
    , maybeUserAuth  = Nothing
    , maybeIgnored   = Nothing
    , maybeBuildDir  = Nothing
    , maybeServerDID = Nothing
    }

instance ToJSON Override where
  toJSON Override {..} = object $ catMaybes
    [ ("peers"      .=) <$> pure peers
    , ("app_url"    .=) <$> maybeAppURL
    , ("user_auth"  .=) <$> maybeUserAuth
    , ("ignore"     .=) <$> maybeIgnored
    , ("build_dir"  .=) <$> maybeBuildDir
    , ("server_did" .=) <$> maybeServerDID
    ]
   
instance FromJSON Override where
  parseJSON = withObject "Override" \obj -> do
    peers          <- obj .:? "peers" .!= []
    maybeAppURL    <- obj .:? "app_url"
    maybeUserAuth  <- obj .:? "user_auth"
    maybeIgnored   <- obj .:? "ignore"
    maybeBuildDir  <- obj .:? "build_dir"
    maybeServerDID <- obj .:? "server_did"
 
    return Override {..}
