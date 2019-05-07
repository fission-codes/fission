{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS.Upload where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import Servant

import           Fission.Env
import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Upload   as Upload
import           Fission.Web.Internal

type API = ReqBody '[JSON, PlainText] Text
        :> Post    '[JSON, PlainText] Text

server :: FissionServer API
server input = Upload.run input >>= \case
  Left  unicodeException -> throwM $ err500 { errBody = foo unicodeException } -- throwIO?
  Right hash            -> return hash

handleError :: Either String Text -> Servant.Handler Text
handleError = \case
  Left _ -> throwError err500
  Right hash -> return hash

api :: Proxy API
api = Proxy

foo :: UnicodeException -> Lazy.ByteString
foo unie = Lazy.fromStrict $ Text.encodeUtf8 $ textDisplay $ displayShow unie -- throwIO?
