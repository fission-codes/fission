{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.MultipartForm () where

import Flow
import RIO

import Control.Lens ((.~), (?~))
import Data.Swagger

import Network.HTTP.Media.MediaType
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal


instance HasSwagger api => HasSwagger (MultipartForm Mem (MultipartData Mem) :> api) where
  toSwagger _ = toSwagger (Proxy @api)
             |> addConsumes ["multipart" // "form-data"]
             |> addParam param
    where
      param = mempty
           |> name        .~ "file"
           |> description ?~ "A file to upload (may also be multipart/form-data)"
           |> required    ?~ True
