{-# LANGUAGE UndecidableInstances #-}

module Fission.Error.AlreadyExists.Types (AlreadyExists (..)) where

import           Servant

import           Fission.Prelude
import           Fission.Web.Error.Class
import           Fission.Models

import qualified Fission.Internal.UTF8 as UTF8

data AlreadyExists entity
  = AlreadyExists
  deriving ( Show
           , Eq
           , Exception
           )

instance Display (AlreadyExists entity) => ToServerError (AlreadyExists entity) where
  toServerError _ = err409 { errBody = UTF8.showLazyBS <| textDisplay (AlreadyExists @entity) }

instance Display (AlreadyExists User) where
  display _ = "User already exists"

instance Display (AlreadyExists LoosePin) where
  display _ = "Loose pin already exists"

instance Display (AlreadyExists Domain) where
  display _ = "Domain already exists"

instance Display (AlreadyExists App) where
  display _ = "App already exists"

instance Display (AlreadyExists HerokuAddOn) where
  display _ = "Heroku add-on already exists"
