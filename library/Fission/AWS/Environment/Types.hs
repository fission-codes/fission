-- | App configuration for AWS
module Fission.AWS.Environment.Types
  ( Environment (..)
  , accessKey
  , secretKey
  ) where

import RIO

import Control.Lens (makeLenses)
import Data.Aeson

import qualified Fission.AWS.Types as AWS
import           Fission.Internal.Orphanage.PGConnectInfo ()

data Environment = Environment
  { _accessKey :: !AWS.AccessKey -- ^ AWS Access Key
  , _secretKey :: !AWS.SecretKey -- ^ AWS Secret Key
  } deriving Show

makeLenses ''Environment

instance FromJSON Environment where
  parseJSON = withObject "AWS.Environment" \obj -> do
    _accessKey <- obj .:? "accessKey" .!= ""
    _secretKey <- obj .:? "secretKey" .!= ""

    return $ Environment {..}
