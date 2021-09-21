module Fission.Web.Server.User.Creator.Error (AlreadyExists (..)) where

import           Servant.Server

import           Fission.Prelude

import           Fission.Key.Asymmetric.Public.Types as Key
import qualified Fission.User.DID.ION.Types          as ION
import           Fission.User.Username.Types

import           Fission.Web.Server.Error

data AlreadyExists
  = ConflictingUsername  Username
  | ConflictingPublicKey Key.Public
  | ConflictingION       ION.ID
  -- TODO | ConflictingEmail Email
  deriving ( Show
           , Eq
           , Exception
           )

instance Display AlreadyExists where
  display = \case
    ConflictingUsername  un  -> "Username "   <> display un  <> " already exists"
    ConflictingPublicKey pk  -> "Public key " <> display pk  <> " already exists"
    ConflictingION       ion -> "ION ID"      <> display ion <> " already exists"

instance ToServerError AlreadyExists where
  toServerError err = err409 { errBody = displayLazyBS err }
