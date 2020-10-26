-- | Fission authorization

module Fission.Authorization.Types
  ( Session (..)
  -- Reexport
  , module Fission.Authorization.Access.Types
  , module Fission.Authorization.Heroku.Types
  ) where

import qualified RIO.Text                                    as Text

import           Fission.Prelude

import           Fission.Models

import           Fission.User.DID

import           Fission.Authorization.Access.Types
import           Fission.Authorization.Heroku.Types

import           Fission.Web.Auth.Token.UCAN.Privilege.Types

import qualified Fission.WNFS.Privilege.Types                as WNFS

-- | The final high-level authorization -- internal use only
data Session = Session
  { requestor :: !(Either Heroku DID)
  , unchecked :: ![Unchecked Privilege]

  , subgraphs :: ![Allowed WNFS.Subgraph]
  , domains   :: ![Allowed Domain]
  , apps      :: ![Allowed App]
  }
  deriving (Show, Eq)

instance Display Session where
  textDisplay = Text.pack . show
