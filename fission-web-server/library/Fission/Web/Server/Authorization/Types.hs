-- | Fission authorization

module Fission.Web.Server.Authorization.Types
  ( Authorization (..)
  , Heroku (..)
  ) where

import qualified RIO.Text                                         as Text

import           Servant.API
import           Servant.Server.Experimental.Auth

import           Web.DID.Types
import           Fission.Web.Auth.Token.UCAN.Potency.Types

import           Fission.Prelude

import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Web.Server.Models

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

type instance AuthServerData (AuthProtect "higher-order") = Authorization
