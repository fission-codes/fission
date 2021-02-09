module Fission.CLI.WebNative.Mutation.Auth.Store.Class (MonadStore (..)) where

import           Network.IPFS.CID.Types
import           RIO.Map

import           Fission.Prelude

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer

class Monad m => MonadStore m where
  insert :: Bearer.Token -> m ()
  getAll :: m (Map CID Bearer.Token)
