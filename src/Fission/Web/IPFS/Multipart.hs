{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Fission.Web.IPFS.Multipart where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Has

import Servant
import Servant.Multipart

import           Fission.Config
import           Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Upload   as Upload
import           Fission.Web.Internal

type API = MultipartForm Mem (MultipartData Mem)
        :> Post '[PlainText] Text

-- instance FromMultipart Mem Utf8Builder where
--     -- fromMultipart form = undefined
--     fromMultipart form@(MultipartData{..}) =
--         case lookupFile "file" form of
--           Nothing                       -> Nothing
--           Just (FileData { fdPayload }) -> Just $ foldr (\chunk acc -> display chunk <> acc) "" $  toStrict fdPayload

server :: (Has IpfsPath cfg, HasLogFunc cfg) => RIOServer cfg API
server form@(MultipartData{..}) =
  Upload.run (decodeUtf8Lenient $ Lazy.toStrict raw) >>= \case
    Left unicodeErr ->
      throwM $ err500 { errBody = UTF8.showLazyBS unicodeErr }

    Right hash -> do
      logInfo $ "Generated hash: " <> display hash
      return hash

  where
    Just (FileData {..}) = lookupFile "file" form
    raw = (fdPayload :: Lazy.ByteString)

api :: Proxy API
api = Proxy
