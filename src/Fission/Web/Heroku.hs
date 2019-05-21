{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.Heroku where

import RIO

import Data.Has
import System.Envy

import Servant.API
import Servant.Client

import Network.HTTP.Client (defaultManagerSettings, newManager)

import Fission.Config
import Fission.Heroku.Provision as Provision

type ProvisionAPI = ReqBody '[JSON] Provision.Request
                 :> Post    '[JSON] Provision

-----------------------

type APIA = "heroku" :> Capture "x" Int :> "resources" :> Get '[JSON] Text
type APIB = "foo" :> Get '[JSON] Text

type API = APIA :<|> APIB

api :: Proxy API
api = Proxy

heroku :: Int -> ClientM Text
foo :: ClientM Text

heroku :<|> foo = client api

-- data BaseUrl = BaseUrl
--   { baseUrlScheme :: Scheme
--   , baseUrlHost   :: String   -- ^ host (eg "haskell.org")
--   , baseUrlPort   :: Int      -- ^ port (eg 80)
--   , baseUrlPath   :: String   -- ^ path (eg "/a/b/c")
--   }

queries :: ClientM (Text, Text)
queries = return ("hi", "there")

run :: IO ()
run = runRIO (defConfig :: Config) $ do
  manager' <- liftIO $ newManager defaultManagerSettings
  resp <- liftIO $ runClientM queries (mkClientEnv manager' (BaseUrl Https "" 443 ""))
  case resp of
    Right (pos, msg) -> do
      logInfo $ displayShow pos
      logInfo $ displayShow msg

    Left err ->
      logError $ displayShow err

-- server :: RIOServer cfg API'
-- server = heroku :<|> foo

-- heroku :: RIOServer cfg APIA
-- heroku _x = return "hi"

-- foo :: RIOServer cfg APIB
-- foo = return "hi"
