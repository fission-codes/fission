module Fission.CLI.Parser.Command.UCAN.Generate.Types (Options (..)) where

import           Web.DID.Types

import           Fission.Prelude

import           Fission.Web.Auth.Token.UCAN.Fact.Types
import           Fission.Web.Auth.Token.UCAN.Potency.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

data Options = Options
  { mayResource :: Maybe (Scope Resource)
  , mayPotency  :: Maybe Potency
  , facts       :: [Fact]
  , audience    :: DID
  , mayNbf      :: Maybe UTCTime
  , mayExp      :: Maybe UTCTime
  } deriving (Show, Eq)
