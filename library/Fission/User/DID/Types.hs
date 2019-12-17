module Fission.User.DID.Types (DID (..)) where

import Fission.Prelude

import Data.Swagger

newtype DID = DID { unDID :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    , ToJSON
                    , FromJSON
                    )
  deriving anyclass ( ToSchema )
