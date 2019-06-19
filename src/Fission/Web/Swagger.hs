module Fission.Web.Swagger
  ( API
  , docs
  ) where

import RIO

import Data.Swagger
import Servant
import Servant.Swagger

import qualified Fission.Web.Routes as Web

type API = "swagger.json" :> Get '[JSON] Swagger

docs :: Swagger
docs = toSwagger (Proxy :: Proxy Web.API)
