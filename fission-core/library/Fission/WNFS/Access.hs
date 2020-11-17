-- |

module Fission.WNFS.Access
  ( login
  ) where

import           Fission.Prelude

import           Fission.User.Username.Types
import qualified Fission.Web.Auth.Token.JWT               as UCAN

import qualified Fission.Authorization.Potency.Types      as WNFS
import qualified Fission.WNFS.Access.Mutation.Store.Class as WNFS.Mutation
import qualified Fission.WNFS.Access.Query.Store.Class    as WNFS.Query

login ::
  ( WNFS.Mutation.Store m
  , WNFS.Query.Store    m
  , MonadRaise          m
  , m `Raises` String
  )
  => Username
  -> Symmetric.Key AES256
  -> UCAN.RawContent
  -> m ()
login username readKey writeUCAN@UCAN.JWT {claims = Claims {resource, potency}} = do
  case (resource, potency) of
    (Complete, SuperUser) -> do
      WNFS.Mutation.Store.insert username "/" potency writeUCAN
      WNFS.Query.Store.insert    username "/" readKey

    _ ->
      raise "Bad resource" -- FIXME better error
