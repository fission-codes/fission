{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage () where

import RIO
import RIO.Orphans ()
import RIO.Process (ProcessContext, HasProcessContext (..))

import Control.Lens

import Data.Has
import Data.Pool
import Data.Swagger

import Network.HTTP.Media.MediaType

import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import qualified Fission.Config        as Config
import qualified Fission.Storage.Types as DB

import Database.Beam
import Database.Beam.Sqlite

import Fission.Log

instance {-# OVERLAPPING #-} Has ProcessContext cfg => HasProcessContext cfg where
  processContextL = hasLens

instance (Has Logger cfg, Has DB.Pool cfg) => MonadBeam Sqlite (RIO cfg) where
  runNoReturn     = nt . runNoReturn
  runReturningOne = nt . runReturningOne

  runReturningMany sql action = do
    cfg <- ask
    let sqlAction = fmap (runRIO cfg) action . nt
    nt $ runReturningMany sql sqlAction

nt :: (Has Logger cfg, Has DB.Pool cfg) => SqliteM a -> RIO cfg a
nt action = do
  DB.Pool pool  <- Config.get
  Logger logger <- Config.get
  let runLogger = runRIO logger . logDebug . displayShow
  liftIO . withResource pool $ \conn ->
    runReaderT (runSqliteM action) (runLogger, conn)

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
