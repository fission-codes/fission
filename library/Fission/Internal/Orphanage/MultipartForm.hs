{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.MultipartForm () where

import Network.HTTP.Media.MediaType
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Fission.Prelude

instance HasSwagger api => HasSwagger (MultipartForm Mem (MultipartData Mem) :> api) where
  toSwagger _ = Proxy @api
             |> toSwagger
             |> addConsumes ["multipart" // "form-data"]
