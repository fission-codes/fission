module Web.JWT.Resolver.Error (Error (..)) where

import           Network.IPFS.CID.Types
import           Servant.Client.Core

import           Fission.Prelude

data Error
  = CannotResolve CID ClientError
  | InvalidJWT ByteString
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    CannotResolve cid err ->
      "Unable to resolve " <> display cid <> " because " <> display err

    InvalidJWT jwtBS ->
      "Invalid resolved JWT: " <> displayBytesUtf8 jwtBS
