module Fission.CLI.Environment.Override.Types (Override (..)) where

import           RIO.Prelude.Types

import           Servant.API

import qualified Network.IPFS.Types                       as IPFS

import           Fission.Prelude

import           Fission.URL
import           Fission.User.DID

import           Fission.Internal.Orphanage.BasicAuthData ()
import           Fission.Internal.Orphanage.Glob.Pattern  ()

-- | This is the part that actually gets written to disk.
--   'Environment' is constructed virtually from layers of 'Override's.
data Override = Override
  { peers          :: ![IPFS.Peer]
  , ipfsIgnored    :: ![Text]
  , maybeAppURL    :: !(Maybe URL)
  , maybeUserAuth  :: !(Maybe BasicAuthData) -- TODO deprecated
  , maybeBuildDir  :: !(Maybe FilePath)
  , maybeServerDID :: !(Maybe DID)
  }

instance Semigroup Override where
  a <> b = Override
    -- Combine
    { peers          = peers          a <>  peers          b
    , ipfsIgnored    = ipfsIgnored    a <>  ipfsIgnored    b

    -- Alternate
    , maybeAppURL    = maybeAppURL    a <|> maybeAppURL    b
    , maybeUserAuth  = maybeUserAuth  a <|> maybeUserAuth  b
    , maybeBuildDir  = maybeBuildDir  a <|> maybeBuildDir  b
    , maybeServerDID = maybeServerDID a <|> maybeServerDID b
    }

instance Monoid Override where
  mempty = Override
    { peers          = []
    , ipfsIgnored    = []

    , maybeAppURL    = Nothing
    , maybeUserAuth  = Nothing
    , maybeBuildDir  = Nothing
    , maybeServerDID = Nothing
    }

instance ToJSON Override where
  toJSON Override {..} = object $ catMaybes
    [ ("peers"      .=) <$> skipEmpty peers
    , ("ignore"     .=) <$> skipEmpty ipfsIgnored

    , ("app_url"    .=) <$> maybeAppURL
    , ("user_auth"  .=) <$> maybeUserAuth
    , ("build_dir"  .=) <$> maybeBuildDir
    , ("server_did" .=) <$> maybeServerDID
    ]
    where
      skipEmpty :: [a] -> Maybe [a]
      skipEmpty [] = Nothing
      skipEmpty xs = Just xs

instance FromJSON Override where
  parseJSON = withObject "Override" \obj -> do
    peers          <- obj .:? "peers"  .!= []
    ipfsIgnored    <- obj .:? "ignore" .!= []

    maybeAppURL    <- obj .:? "app_url"
    maybeUserAuth  <- obj .:? "user_auth"
    maybeBuildDir  <- obj .:? "build_dir"
    maybeServerDID <- obj .:? "server_did"

    return Override {..}
