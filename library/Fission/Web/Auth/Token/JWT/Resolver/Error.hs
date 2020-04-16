module Fission.Web.Auth.Token.JWT.Resolver.Error (Error (..)) where

import           Servant.Server

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Process.Error as IPFS.Process

import           Fission.Prelude
import           Fission.Web.Error.Class

data Error
  = CannotResolve CID IPFS.Process.Error
  | InvalidJWT ByteString
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    CannotResolve cid err ->
      "Unable to resolve " <> display cid <> " because " <> display err

    InvalidJWT jwtBS ->
      "Invalid resolved JWT: " <> displayBytesUtf8 jwtBS

instance ToServerError Error where
  toServerError = \case
    err@(CannotResolve _ _) -> err504 { errBody = displayLazyBS err }
    err@(InvalidJWT _)      -> err422 { errBody = displayLazyBS err }
