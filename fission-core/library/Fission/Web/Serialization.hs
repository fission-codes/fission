module Fission.Web.Serialization
  ( parseHeader
  , module Fission.Web.Serialization.Error
  ) where

import qualified Servant.API                     as Servant

import           Fission.Prelude

import           Fission.Web.Serialization.Error

parseHeader :: Servant.FromHttpApiData a => ByteString -> Either Error a
parseHeader bs =
  case Servant.parseHeader bs of
    Left err  -> Left $ DeserializationError err
    Right val -> Right val
