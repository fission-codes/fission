module Fission.CLI
  ( cli
  , ClientRunner (..)
  , Config (..)
  ) where

import           RIO
import           RIO.ByteString
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Directory
import           RIO.File
import           RIO.FilePath
import qualified RIO.List as List

import           Data.Aeson
import           Data.Aeson.Encoding
import qualified Data.ByteString.Char8 as BS
import           Data.Has
import qualified Data.Yaml as Yaml

import Control.Concurrent
import Control.Lens (makeLenses)

-- import System.FSNotify

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
import           Fission.Internal.Constraint
import           Fission.CLI.Types
import qualified Fission.Config as Config
import           Fission.Environment
import           Fission.Web.Auth.Client as Fission.Auth

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
       => MonadUnliftIO        m
       => HasLogFunc       cfg
       => Has ClientRunner cfg
       => m ()
login' = do
  logDebug "Starting login sequence"
  putStr "Username: "
  username <- getLine
  rawPass  <- liftIO . runInputT defaultSettings $ getPassword (Just '•') "Password: "

  case rawPass of
    Nothing -> putStr "Unable to read password"
    Just password -> do
      logDebug "Attempting API verification"
      ClientRunner runner <- Config.get
      let auth = BasicAuthData username $ BS.pack password

      liftIO ANSI.hideCursor

      liftIO $ ANSI.cursorForward 3
      liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
      putStr "Verifying your credentials..."
      liftIO $ ANSI.setCursorColumn 0

      result <- liftIO . withLoader $ runner $ Fission.Auth.verify auth

      liftIO ANSI.showCursor

      case result of
        Right _ok -> do
          home <- getHomeDirectory
          let path = home </> ".fission.yaml"
          writeBinaryFileDurable path $ Yaml.encode auth

          liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
          putText $ Emoji.whiteHeavyCheckMark <> " Logged in successfully!"

        Left err -> do
          logDebug $ displayShow err
          liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
          putText $ Emoji.prohibited <> " Authorization failed"

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
loading = forever
        . (const prep <=< sequence_)
        . List.intersperse prep
        $ fmap putText
            [ Emoji.clock0100
            , Emoji.clock0200
            , Emoji.clock0300
            , Emoji.clock0400
            , Emoji.clock0500
            , Emoji.clock0600
            , Emoji.clock0700
            , Emoji.clock0800
            , Emoji.clock0900
            , Emoji.clock1000
            , Emoji.clock1100
            , Emoji.clock1200
            ]

-- put :: MonadIO m => Puttable txt => txt -> m ()

putText :: MonadIO m => Text -> m ()
putText = putStr . encodeUtf8

withLoader :: IO a -> IO a
withLoader = RIO.bracket (forkIO loading) cleanup . const
  where
    cleanup :: ThreadId -> IO ()
    cleanup pid = do
      killThread pid
      reset

reset :: IO ()
reset = do
  ANSI.cursorBackward 4
  ANSI.clearLine

prep :: IO ()
prep = do
  RIO.threadDelay delay
  ANSI.cursorBackward 4

delay :: Int
delay = 50000

noop :: Applicative m => m ()
noop = pure ()
