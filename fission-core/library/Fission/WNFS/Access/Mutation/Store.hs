module Fission.WNFS.Access.Mutation.Store
  ( getRootProof
  -- * Reexports
  , module Fission.WNFS.Access.Mutation.Store.Class
  ) where

import qualified RIO.Map                                  as Map

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import           Fission.User.Username.Types
import qualified Fission.Web.Auth.Token.JWT               as UCAN

import qualified Fission.WNFS.Access.Mutation.Store.Class as Mutation

-- Reexports

import           Fission.WNFS.Access.Mutation.Store.Class

getRootProof ::
  ( Mutation.Store m
  , MonadRaise     m
  , m `Raises` NotFound UCAN.JWT
  , m `Raises` String -- FIXME
  )
  => Username
  -> m UCAN.Proof
getRootProof username =
  Mutation.getRootKey username >>= \case
    Right _ ->
      return UCAN.RootCredential

    Left _ -> do
      table <- getCIDsFor username "/"
      case Map.toList table of
        [] ->
          raise $ NotFound @UCAN.JWT

        ((_, cid) : _) -> do
          (raw, ucan) <- ensureM $ getByCID cid
          return $ UCAN.Nested raw ucan
