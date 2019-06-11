module Main (main) where

import RIO
import RIO.Char (toLower)

import Data.Aeson

import Network.Wai.Handler.Warp
import Network.Wai.Logger

import System.Environment

import Fission.Config         as Config
import Fission.Storage.SQLite as SQLite

import Database.Selda ()
import Fission.Internal.Orphanage ()

import qualified Fission.Log        as Log
import qualified Fission.Monitor    as Monitor
import qualified Fission.Web        as Web
import           Fission.Web.Config as Web.Config
import           Fission.Platform.Heroku.AddOn.Manifest as Manifest

main :: IO ()
main = withStdoutLogger $ \stdOut -> do
  Just (manifest :: Manifest) <- decodeFileStrict "./addon-manifest.json"
  monitorEnv <- lookupEnv "MONITOR"
  monitored  <- return $ maybe False ((== "true") . fmap toLower) monitorEnv

  runRIO (mkLogFunc Log.simple) do
    Web.Config.Config {port} <- Web.Config.get
    pool <- SQLite.connPool $ DBPath "ipfs-api.sqlite" -- TODO make env var

    let
      portSettings = setPort port
      logSettings  = setLogger stdOut
      settings     = portSettings $ logSettings defaultSettings

      hID   = HerokuID       . encodeUtf8 $ manifest ^. Manifest.id
      hPass = HerokuPassword . encodeUtf8 $ manifest ^. api ^. password
      cfg   = Config.base hID hPass (DBPool pool)

    runRIO cfg do
      when monitored Monitor.wai
      logInfo $ "Servant running at port " <> display port
      liftIO . runSettings settings =<< Web.app cfg
