-- | Fission authorization

module Fission.Authorization.Types
  ( Authorization (..)
  -- Reexport
  , module Fission.Authorization.Access.Types
  , module Fission.Authorization.Heroku.Types
  ) where

import qualified RIO.Text                           as Text

import           Fission.Prelude

import           Fission.User.DID

import           Fission.Authorization.Access.Types
import           Fission.Authorization.Heroku.Types

-- | The final high-level authorization -- internal use only
data Authorization privilege = Authorization
  { sender :: !(Either Heroku DID)
  , access :: ![Access privilege]
  } deriving (Show, Eq)

instance Show privilege => Display (Authorization privilege) where
  textDisplay = Text.pack . show
