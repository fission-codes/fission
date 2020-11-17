module Fission.WNFS.Access.Mutation.Store.Class (Store (..)) where

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import           Fission.User.Username.Types

import qualified Fission.Web.Auth.Token.JWT                       as UCAN

import qualified Fission.Authorization.Potency.Types              as WNFS
import qualified Fission.WNFS.Access.Mutation.Authorization.Types as WNFS

class Monad m => Store m where
  lookup ::
       Username     -- ^ Registered user
    -> FilePath     -- ^ File path or bare namefilter
    -> WNFS.Potency -- ^ Minimum potency requested
    -> m (Either (NotFound WNFS.Authorization) WNFS.Authorization)

  insert :: Username -> UCAN.RawContent -> m ()

-- FIXME add to rescue:
-- foo :: Maybe a -> (err :: * -> *) -> Either (err a) a
