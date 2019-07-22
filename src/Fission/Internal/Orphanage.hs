{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage () where

import GHC.Stack

import RIO
import RIO.Orphans ()
import qualified RIO.Partial as Partial

import Control.Lens

import Data.Aeson.Types
import Data.Scientific
import Data.Has
import Data.Pool
import Data.Swagger
import Data.UUID as UUID

-- import Database.Selda
-- import Database.Selda.Backend

import Network.HTTP.Media.MediaType

import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import qualified Fission.Config        as Config
import qualified Fission.Storage.Types as DB

import Database.Beam
import Database.Beam.Backend
import Database.Beam.Sqlite

import Fission.Internal.Constraint
import Fission.Config.Types

instance Enum    UUID
-- instance SqlType UUID

instance Bounded UUID where
  minBound = Partial.fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"
  maxBound = Partial.fromJust $ UUID.fromString "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"

-- instance Display (ID a) where
--   display = display . fromId

-- instance ToJSON (ID a) where
--   toJSON = Number . fromIntegral . fromId

-- instance FromJSON (ID a) where
--   parseJSON = \case
--     num@(Number n) ->
--       case toBoundedInteger n of
--         Nothing -> errMsg num
--         Just i  -> return $ toId i

--     invalid ->
--       errMsg invalid

--     where
--       errMsg = modifyFailure ("parsing ID failed, " <>) . typeMismatch "Number"

-- instance MonadBeamInsertReturning Sqlite (RIO cfg) where
--   runInsertReturningList = nt . runInsertReturningList

instance (HasLogFunc cfg, Has DB.Pool cfg) => MonadBeam Sqlite (RIO cfg) where
  runNoReturn = nt . runNoReturn
  runReturningOne = nt . runReturningOne
  runReturningMany sql action = do
    cfg <- ask
    let sqlAction = fmap (runRIO cfg) action . nt
    nt $ runReturningMany sql sqlAction

nt :: (HasLogFunc cfg, Has DB.Pool cfg) => SqliteM a -> RIO cfg a
nt action = do
  DB.Pool pool <- Config.get
  logger <- RIO.view logFuncL
  let runLogger = runRIO logger . logDebug . displayShow
  liftIO . withResource pool $ \conn ->
    runReaderT (runSqliteM action) (runLogger, conn)

instance HasLogFunc (LogFunc, b) where
  logFuncL = _1

instance HasLogFunc (LogFunc, b, c) where
  logFuncL = _1

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
