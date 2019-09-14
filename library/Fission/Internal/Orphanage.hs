{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage () where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Orphans ()
import qualified RIO.Partial         as Partial

import Control.Lens

import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import Data.Has
import Data.Pool
import Data.Swagger
import Data.UUID as UUID

import Database.Selda
import Database.Selda.Backend

import Network.HTTP.Media.MediaType
import Network.HTTP.Types.Status

import Servant
import Servant.Client as C
import Servant.Exception
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import qualified Fission.Config        as Config
import qualified Fission.Storage.Types as DB

instance Enum    UUID
instance SqlType UUID

instance Bounded UUID where
  minBound = Partial.fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"
  maxBound = Partial.fromJust $ UUID.fromString "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"

instance Display Natural where
  display nat = display (fromIntegral nat :: Integer)

instance Display (ID a) where
  display = display . fromId

instance Display ServantError where
  display = \case
    FailureResponse C.Response {responseBody} ->
      displayShow responseBody

    DecodeFailure txt C.Response {responseBody} ->
      display txt <> " - " <> displayShow responseBody

    UnsupportedContentType _ C.Response {responseBody} ->
      displayShow responseBody

    InvalidContentTypeHeader resp ->
      displayShow resp

    ConnectionError txt ->
      display txt

instance ToJSON ServantError where
  toJSON = String . textDisplay

instance ToServantErr ServantError where
  status = \case
    FailureResponse          _   -> status400
    DecodeFailure            _ _ -> status400
    UnsupportedContentType   _ _ -> status415
    InvalidContentTypeHeader _   -> status415
    ConnectionError          _   -> status500

  message = textDisplay

instance ToJSON (ID a) where
  toJSON = Number . fromIntegral . fromId

instance FromJSON (ID a) where
  parseJSON = \case
    num@(Number n) ->
      case toBoundedInteger n of
        Nothing -> errMsg num
        Just i  -> return $ toId i

    invalid ->
      errMsg invalid

    where
      errMsg = modifyFailure ("parsing ID failed, " <>) . typeMismatch "Number"

instance Has DB.Pool cfg => MonadSelda (RIO cfg) where
  seldaConnection = do
    DB.Pool pool <- Config.get
    liftIO $ withResource pool pure

instance HasLogFunc (LogFunc, b) where
  logFuncL = _1

instance HasLogFunc (LogFunc, b, c) where
  logFuncL = _1

instance MimeRender PlainText Lazy.ByteString where
  mimeRender _proxy = id

instance MimeRender PlainText a => MimeRender PlainText [a] where
  mimeRender proxy values = "["<> meat <>"]"
    where
      meat :: Lazy.ByteString
      meat =  Lazy.intercalate "," $ mimeRender proxy <$> values

instance ToJSON BasicAuthData where
  toJSON (BasicAuthData username password) =
    Object [ ("username", String $ decodeUtf8Lenient username)
           , ("password", String $ decodeUtf8Lenient password)
           ]

instance HasSwagger api => HasSwagger (BasicAuth x r :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)
              & securityDefinitions .~ [("basic", SecurityScheme SecuritySchemeBasic Nothing)]

instance HasSwagger api => HasSwagger (MultipartForm Mem (MultipartData Mem) :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)
              & addConsumes ["multipart" // "form-data"]
              & addParam param
    where
      param = mempty
            & name        .~ "file"
            & description ?~ "A file to upload (may also be multipart/form-data)"
            & required    ?~ True
