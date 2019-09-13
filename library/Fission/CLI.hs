module Fission.CLI
  ( cli
  , ClientRunner (..)
  , Config (..)
  ) where

import RIO
import RIO.ByteString

import Control.Lens (makeLenses)
import qualified Fission.Config as Config

import Data.Has

-- import System.FSNotify

import qualified Data.ByteString.Char8 as BS

import Servant
import Servant.Client
import Servant.API

import qualified Network.HTTP.Client as HTTP

import qualified System.Console.ANSI as ANSI

import           System.Console.Haskeline
import qualified System.Console.Haskeline.MonadException as HL

import Options.Applicative as OA
import Options.Applicative.Simple

import qualified Fission.Emoji as Emoji
import Fission.Internal.Constraint
import Fission.CLI.Types

import Fission.Environment
import Fission.Web.Auth.Client as Fission.Auth

newtype ClientRunner = ClientRunner
  { getRunner :: forall a. ClientM a -> IO (Either ServantError a) }

data Config = Config
  { _fissionAPI :: !ClientRunner
  , _logFunc    :: !LogFunc
  }

makeLenses ''Config

instance HasLogFunc Config where
  logFuncL = logFunc

instance Has ClientRunner Config where
  hasLens = fissionAPI

cli :: MonadIO m => Config -> IO ((), m ())
cli cfg =
  simpleOptions version description detail noop do
    login cfg
    greet
    print
    exit
  where
    version     = "0.0.1"
    description = "Top lines about what the CLI is for"
    detail      = "This CLI does some cool stuff"

login :: MonadIO m => Config -> CommandM (m ())
login cfg =
  addCommand
    "login"
    "Add your Fission credentials"
    (const $ runRIO cfg login')
    noop

login' :: MonadRIO         cfg m
       => HasLogFunc       cfg
       => Has ClientRunner cfg
       => m ()
login' = do
  logDebug "Starting login sequence"

  putStr "Username: "
  username <- getLine
  rawPass  <- liftIO . runInputT defaultSettings $
    getPassword (Just '•') "Password: "

  case rawPass of
    Nothing  -> putStr "Unable to read password"
    Just password -> do
      putStr "\n"
      logDebug "Attempting API verification"
      ClientRunner runner <- Config.get
      let auth = BasicAuthData username $ BS.pack password

      liftIO (runner $ Fission.Auth.verify auth) >>= \case
        Right _ok ->
          putStr $ encodeUtf8 Emoji.okHand <> " Logged in as " <> username

        Left err -> do
          logDebug $ displayShow err
          putStr $ encodeUtf8 Emoji.prohibited <> " Authorization failed"

greet :: MonadIO m => CommandM (m ())
greet =
  addCommand
    "greet"
    "Greet the user"
    (\opts -> putStr $ "Hello " <> opts)
    (strOption $ mconcat
      [ long "name"
      , short 'n'
      , metavar "USER_NAME"
      ]
    )

print :: MonadIO m => CommandM (m ())
print =
  addCommand
    "print"
    "Run the printer"
    (const printer)
    noop

exit :: MonadIO m => CommandM (m ())
exit =
  addSubCommands
    "exit"
    "Just exit"
    do
      addCommand
        "now"
        "Exit immedietly"
        (\_ -> putStr "exiting!\n")
        noop

      addCommand
        "later"
        "Exit soon"
        (\_ -> putStr "exiting...\n")
        noop

printer :: MonadIO m => m ()
printer = do
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  putStr $ "Hello world \xe2\x9c\x8b"
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  putStr $ "\nDone" <> encodeUtf8 Emoji.rocket <> "\n"
  liftIO loading

loading :: IO ()
loading = forever do
  putStr "\xE2\x96\x98"-- "▝"
  reset
  putStr "\xE2\x96\x9D" -- "▘"
  reset
  putStr "\xE2\x96\x97" -- "▗"
  reset
  putStr "\xE2\x96\x96" -- "▖"
  reset

reset :: IO ()
reset = do
  threadDelay delay
  ANSI.cursorBackward 4

delay :: Int
delay = 50000

noop :: Applicative m => m ()
noop = pure ()
