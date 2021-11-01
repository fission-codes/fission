module Network.IPFS.Client.Error.Types (ErrorBody (..)) where

import Network.IPFS.Prelude

data ErrorBody = ErrorBody {message :: String}

instance FromJSON ErrorBody where
  parseJSON = withObject "ErrorBody" \obj -> do
    message    <- obj .: "Message"
    return ErrorBody {..}
