module Fission.Web.Swagger
  ( API
  , server
  ) where

import Data.Swagger
import Servant

import Fission.Web.Server

import qualified Fission.Web.Swagger.Public as Docs.Public
import qualified Fission.Web.Swagger.Complete as Docs.Complete

type API = Docs.Public.API :<|> Docs.Complete.API

server :: Host -> RIOServer cfg API
server hst = Docs.Public.server hst :<|> Docs.Complete.server hst
