module Fission.User.DID.Types (DID (..)) where

import Fission.Prelude

import Data.Swagger

newtype DID = DID { unDID :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToJSON
                    , FromJSON
                    , ToSchema
                    )
