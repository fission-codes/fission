-- | Fission authorization

module Fission.Authorization.Types
  ( Authorization (..)
  , Heroku (..)
  ) where

import qualified RIO.Text                                   as Text

import           Fission.Prelude

import           Fission.Models
import           Fission.User.DID

import           Fission.Web.Auth.Token.UCAN.Resource.Types

data Heroku = Heroku
  deriving (Show, Eq)

-- | The final high-level authorization -- internal use only
data Authorization entity = Authorization
  { sender :: !(Either Heroku DID)
  , about  :: !(Entity User)
  , rights :: ![Resource] -- FIXME
  } deriving (Show, Eq)

instance Show entity => Display (Authorization entity) where
  textDisplay = Text.pack . show
