{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS.Multipart where

import RIO

import Servant
import Servant.Multipart

import Fission.Web.Internal

type API = MultipartForm Tmp (MultipartData Tmp)
        :> Post '[PlainText] Text

server :: RIOServer cfg API
server multipartData = do
  return str

  where str = "The form was submitted with "
                <> textDisplay nInputs <> " textual inputs and "
                <> textDisplay nFiles  <> " files."
        nInputs = length (inputs multipartData)
        nFiles  = length (files multipartData)

api :: Proxy API
api = Proxy
