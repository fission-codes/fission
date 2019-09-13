module Main (main) where

import           RIO
import qualified RIO.Partial as Partial

import System.Environment (lookupEnv)

import Data.Has

-- import System.FSNotify

import qualified Data.ByteString.Char8 as BS

import Servant
import Servant.API
import Servant.Client

import qualified Network.HTTP.Client as HTTP

import qualified System.Console.ANSI as ANSI

import           System.Console.Haskeline
import qualified System.Console.Haskeline.MonadException as HL

import Options.Applicative        as OA
import Options.Applicative.Simple

import           Fission.CLI.Types
import qualified Fission.Emoji               as Emoji
import           Fission.Internal.Constraint

import Fission.Environment
import Fission.Web.Auth.Client as Fission.Auth

import Fission.CLI

main :: IO ()
main = do
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  verbose     <- isJust <$> lookupEnv "RIO_VERBOSE"
  logOptions  <- logOptionsHandle stderr verbose

  isTLS <- getFlag "FISSION_TLS" -- TODO default to true
  path  <- withEnv "FISSION_ROOT" "" id
  host  <- withEnv "FISSION_HOST" "localhost" id -- TODO default to prod
  port  <- withEnv "FISSION_PORT" (if isTLS then 80 else 443) Partial.read

  let scheme = if isTLS then Https else Http
  let url    = BaseUrl scheme host port path

  withLogFunc logOptions $ \logger -> do
    let cfg = Config
                { _fissionAPI = ClientRunner $ Fission.Auth.run httpManager url
                , _logFunc    = logger
                }
    (_, run) <- cli cfg
    run
