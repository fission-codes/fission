-- | Fission authorization sessions
module Fission.Authorization.Session.Types
  ( Session (..)
  -- Reexport
  , module Fission.Authorization.Heroku.Types
  , module Fission.Authorization.Access.Unchecked.Types
  ) where

import qualified RIO.Text                                     as Text

import           Fission.Prelude

import           Fission.Models

import           Fission.User.DID

import           Fission.Authorization.Allowable.Class
import           Fission.Authorization.Grantable.Class
import           Fission.Authorization.Heroku.Types

import           Fission.Authorization.Access.Unchecked.Types
import           Fission.Web.Auth.Token.UCAN.Privilege.Types

import qualified Fission.WNFS.Privilege.Types                 as WNFS

-- | The final high-level authorization
data Session = Session
  { requestor :: !(Either Heroku DID)
  , unchecked :: ![Unchecked Privilege]

  , subgraphs :: ![Access WNFS.Subgraph]
  , domains   :: ![Access Domain]
  , apps      :: ![Access App]
  }
  deriving (Show, Eq)

instance Display Session where
  textDisplay = Text.pack . show
