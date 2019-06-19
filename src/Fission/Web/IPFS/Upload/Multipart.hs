module Fission.Web.IPFS.Upload.Multipart
  ( API
  , add
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Servant
import Servant.Multipart

import Control.Lens
import Data.Swagger
import Servant.Swagger
import Servant.Swagger.Internal (addParam)

import           Fission.Web.Server
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Storage.IPFS as Storage.IPFS

type API = MultipartForm Mem (MultipartData Mem)
        :> Post '[OctetStream, PlainText] IPFS.Address

add :: Has IPFS.Path     cfg
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => RIOServer         cfg API
add form =
  case lookupFile "file" form of
    Just FileData { fdPayload } -> do
      hash <- Storage.IPFS.add fdPayload
      logInfo $ "Generated address: " <> display hash
      return hash

    Nothing ->
      throwM $ err422 { errBody = "File not processable" }

instance HasSwagger api => HasSwagger (MultipartForm Mem (MultipartData Mem) :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api) --  & addParam param
    -- where
    --   param = mempty
    --         & name        .~ "file"
    --         & required    ?~ True
    --         & description ?~ "File to upload"
    --         & schema      .~ ParamOther
    --             $ mempty
    --             & in_ .~ ParamFormData
    --             & paramSchema .~ (mempty & type_ .~ SwaggerFile)
