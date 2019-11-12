{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.MultipartForm () where

import Data.Swagger
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
             |> addParam param
    where
      param = mempty
           |> name        .~ "file"
           |> description ?~ "A file to upload (may also be multipart/form-data)"
           |> required    ?~ True
