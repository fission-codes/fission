module Fission.Error.NotFound.Types (NotFound (..)) where

import           Servant

import           Fission.Prelude
import           Fission.Web.Error.Class
import           Fission.Models

data NotFound entity
  = NotFound
  deriving ( Show
           , Eq
           , Exception
           )

instance ToServerError (NotFound entity) where
  toServerError _ = err404

instance Display (NotFound User) where
  display _ = "User not found"

instance Display (NotFound LoosePin) where
  display _ = "Loose pin not found"

instance Display (NotFound Domain) where
  display _ = "Domain not found"

instance Display (NotFound App) where
  display _ = "App not found"
