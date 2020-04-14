-- | Fission authorization

module Fission.Authorization.Types (Authorization (..)) where

import qualified RIO.Text as Text

import           Fission.Prelude

import           Fission.User.DID
import           Fission.Models

import           Fission.Authorization.Potency.Types

-- | The final high-level authorization
data Authorization = Authorization
  { sender  :: !DID
  , about   :: !User
  , potency :: !Potency
  , scope   :: !Text -- May later be a Unix path
  }
  deriving (Show, Eq)

instance Display Authorization where
  textDisplay = Text.pack . show
