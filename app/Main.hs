module Main where

import RIO

import           Fission
import qualified Fission.Ambient as Ambient
import qualified Fission.Env     as Env
import qualified Fission.Web     as Web

main :: IO ()
main = do
  -- runEnv  <- Ambient.lookup "ENV" Env.Development
  port <- Ambient.lookup "PORT" 8000

  -- let logger = setLogger env
  --     env    = Env.Env { Env._logger = mkLogFunc Env.simpleLogger }

  runRIO Env.base (Web.app `startAtPort` port)

--   pool <- makePool env

--   runSqlPool doMigrate pool
--   seed env

--   let logger = setLogger env
--       config = Config { getPool = pool
--                       , getEnv  = env
--                       }

--   putStrLn "Up and running"
-- run port . logger $ app config
