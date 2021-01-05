-- | Fission authorization

module Fission.Web.Server.Authorization.Types
  ( Authorization (..)
  , Heroku (..)
  ) where

import qualified RIO.Text                                                as Text

import           Fission.Prelude

import           Fission.User.DID

import           Fission.Web.Server.Models

import           Fission.Web.Server.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Server.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Server.Authorization.Potency.Types

data Heroku = Heroku
  deriving (Show, Eq)

-- | The final high-level authorization -- internal use only
data Authorization = Authorization
  { sender   :: Either Heroku DID
  , about    :: Entity User
  , potency  :: Potency
  , resource :: Scope Resource
  } deriving (Show, Eq)

instance Display Authorization where
  textDisplay = Text.pack . show
