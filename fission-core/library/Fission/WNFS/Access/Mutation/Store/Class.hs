module Fission.WNFS.Access.Mutation.Store.Class (Store (..)) where

import           Network.IPFS.CID.Types
import           RIO.Map

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import           Fission.User.Username.Types

import qualified Fission.Web.Auth.Token.JWT                       as UCAN

import qualified Fission.Authorization.Potency.Types              as WNFS
import qualified Fission.WNFS.Access.Mutation.Authorization.Types as WNFS

class Monad m => Store m where
  insert     :: Username -> UCAN.RawContent -> m ()
  getCIDsFor :: Username -> FilePath -> m (Map FilePath CID)
  getByCID   :: CID -> m (Either (NotFound UCAN.JWT) UCAN.JWT)
