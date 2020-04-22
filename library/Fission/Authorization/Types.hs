-- | Fission authorization

module Fission.Authorization.Types
  ( Authorization (..)
  , Heroku (..)
  ) where

import qualified RIO.Text as Text

import           Fission.Prelude

import           Fission.User.DID
import           Fission.Models

import           Fission.Authorization.Potency.Types

data Heroku = Heroku
  deriving (Show, Eq)

-- | The final high-level authorization -- internal use only
data Authorization = Authorization
  { sender  :: !(Either Heroku DID)
  , about   :: !(Entity User)
  , potency :: !Potency
  , scope   :: !Text -- May later be a POSIX-style path
  } deriving (Show, Eq)

instance Display Authorization where
  textDisplay = Text.pack . show
