{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Fission where

import RIO
import RIO.ByteString as BS
import RIO.Text as Text

import Control.Lens

import Network.Wai                          (Middleware, Application)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Handler.Warp

import System.Environment (lookupEnv)

import Fission.Internal.Constraint (WithRIO, Loggable)

--------------------------------------------------------------------------------

-- | Top-level application type
type Fission = RIO Env ()

--------------------------------------------------------------------------------

data Env = Env
  { _logger :: LogFunc
  }

makeLenses ''Env

instance HasLogFunc Env where
  logFuncL = logger

--------------------------------------------------------------------------------

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data RunEnvironment
  = Test
  | Development
  | Production
  deriving ( Eq
           , Show
           , Read
           )

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: RunEnvironment -> Middleware
setLogger = \case
  Test        -> id
  Development -> logStdoutDev
  Production  -> logStdout

--------------------------------------------------------------------------------

lookupAmbient :: (Read a, MonadIO m) => String -> a -> m a
lookupAmbient key fallback = do
  maybeValue <- liftIO $ lookupEnv key

  case maybeValue of
    Nothing  -> return fallback
    Just str ->
      maybe
        (error $ "Failed to read [[" <> str <> "]] for environment variable " <> key)
        return
        (readMaybe str)

defaultEnv :: Env
defaultEnv = Env
  { _logger = mkLogFunc simpleLogger
  }

startAtPort :: (WithRIO m env, Loggable env) => Application -> Word -> m ()
startAtPort webApp port = do
  logInfo $ "Servant running at port " <> display port
  liftIO $ fromIntegral port `run` webApp

startApp :: (WithRIO m env, Loggable env) => Application -> m ()
startApp webApp = webApp `startAtPort` 8000

--------------------------------------------------------------------------------

simpleLogger :: CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()
simpleLogger _ src _ msg =
  BS.putStr $ Text.encodeUtf8 $ mconcat
    [ "***"
    -- , textDisplay lvl
    , "*** "
    , textDisplay src
    , " | "
    , textDisplay msg
    ]
