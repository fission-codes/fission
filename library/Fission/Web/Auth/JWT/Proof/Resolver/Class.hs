module Fission.Web.Auth.JWT.Proof.Resolver.Class
  ( Resolver (..)
  , Error    (..)
  ) where

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Process.Error as IPFS.Process

import           Fission.Prelude
import           Fission.Web.Auth.JWT.Types

-- FIXME doesn';t need to live under Proof!
class Monad m => Resolver m where
  resolve :: CID -> m (Either Error (ByteString, JWT))
 
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
