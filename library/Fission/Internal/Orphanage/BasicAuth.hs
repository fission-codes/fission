{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.BasicAuth () where

import RIO

import Control.Lens
import Data.Swagger
import Servant
import Servant.Swagger

instance HasSwagger api => HasSwagger (BasicAuth x r :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)
              & securityDefinitions .~ [("basic", SecurityScheme SecuritySchemeBasic Nothing)]
