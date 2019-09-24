module Main (main) where

import           RIO
import           RIO.Process (mkDefaultProcessContext)

import qualified Data.Aeson as JSON
import qualified Data.Yaml  as YAML

import qualified Network.HTTP.Client as HTTP
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.RequestLogger

import           SuperRecord as SR

import           Fission.Internal.Orphanage.Rec ()
import           Fission.Internal.Orphanage.RIO ()

import qualified Fission.Monitor            as Monitor
import           Fission.Storage.PostgreSQL (connPool)

import qualified Fission.Web      as Web
import qualified Fission.Web.CORS as CORS
import qualified Fission.Web.Log  as Web.Log

import Fission.Platform.Heroku.AddOn.Manifest
-- import qualified Fission.Platform.Heroku.Types          as Hku

import           Fission.Environment
import           Fission.Environment.Types as Env
-- import           Fission.IPFS.Config.Types    as IPFS
-- import qualified Fission.Storage.Environment.Types as Storage
-- import qualified Fission.Web.Environment.Types     as Web
-- import qualified Fission.Web.Environment.Types     as WebEnv

main :: IO ()
main = do
  Just (heroku :: Manifest) <- JSON.decodeFileStrict "./addon-manifest.json"
  -- Right Environment {_web, _storage, _ipfs} <- YAML.decodeFileEither "./env.yaml"

  YAML.decodeFileEither "./env.yaml" >>= \case
    Left err -> error $ show err
    Right (env :: Rec Env.Fields) -> do
      let
        baseCfg =   (env ^. SR.lens #web)
          `combine` (env ^. SR.lens #storage)
          `combine` (env ^. SR.lens #ipfs)

      isVerbose  <- getFlag "RIO_VERBOSE" .!~ False
      logOptions <- logOptionsHandle stdout isVerbose

      withLogFunc (setLogUseTime True logOptions) \logFunc -> do
        processCtx  <- mkDefaultProcessContext
        httpManager <- HTTP.newManager HTTP.defaultManagerSettings
        dbPool      <- runRIO (env ^. SR.lens #storage) connPool

        let
          cfg =  #processCtx     := processCtx
            SR.& #httpManager    := httpManager
            SR.& #dbPool         := dbPool
            SR.& #herokuID       := (SR.get #id heroku)
            SR.& #herokuPassword := (heroku ^. SR.lens #api . SR.lens #password)
            SR.& #logFunc        := logFunc
            SR.& baseCfg

        runRIO cfg do
          -- logDebug $ displayShow visibleRec

          let
            webLogger = Web.Log.mkSettings logFunc (env ^. SR.lens #web . SR.lens #port)
            runner    = if env ^. SR.lens #web . SR.lens #tls    then runTLS tlsSettings' else runSettings
            debugger  = if env ^. SR.lens #web . SR.lens #pretty then id                  else logStdoutDev

          when (env ^. SR.lens #web . SR.lens #monitor) Monitor.wai
          liftIO . runner webLogger
                . CORS.middleware
                . debugger
                =<< Web.app
                =<< ask

tlsSettings' :: TLSSettings
tlsSettings' = tlsSettings "domain-crt.txt" "domain-key.txt"
