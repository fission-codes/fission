-- | App configuration for IPFS
module Fission.IPFS.Config.Types (Config, Fields) where -- (Config (..)) where

import           RIO hiding (timeout)

-- import           Data.Aeson
import qualified Servant.Client as Client
import           SuperRecord    as SR

import           Fission.Internal.Orphanage.PGConnectInfo ()

type Fields = '[ "ipfsPath"    := FilePath
               , "ipfsTimeout" := Natural
               , "ipfsURL"     := Client.BaseUrl
               ]

type Config cfg = HasOf Fields cfg

-- instance FromJSON Config where
--   parseJSON = withObject "IPFS.Environment" \obj -> do
--     timeout <- obj .:? "timeout" .!= 3600
--     binPath <- obj .:? "binPath" .!= "/usr/local/bin/ipfs"
--     url     <- obj .:  "url"

--     return . Config
--            $ #ipfsPath    := binPath
--         SR.& #ipfsURL     := url
--         SR.& #ipfsTimeout := timeout
--         SR.& rnil
