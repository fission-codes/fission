module Fission.CLI
  ( cli
  , commands
  ) where

import RIO
import RIO.ByteString

-- import System.FSNotify

import qualified System.Console.ANSI as ANSI

import           System.Console.Haskeline
import qualified System.Console.Haskeline.MonadException as HL

import Options.Applicative as OA
import Options.Applicative.Simple

import qualified Fission.Emoji as Emoji
import Fission.Internal.Constraint
import Fission.CLI.Types

-- instance HL.MonadException (RIO cfg) where
--   controlIO = ReaderT

cli :: CommandM (m ()) -> IO ((), m ())
cli cmds =
  simpleOptions
    "0.0.1"
    "Top lines about what the CLI is for"
    "This CLI does some cool stuff"
    noop
    cmds

commands :: MonadIO m => CommandM (m ())
commands = do
  login
  greet
  print
  exit

login :: MonadIO m => CommandM (m ())
login =
  addCommand
    "login"
    "Add your Fission credentials"
    (const $ runSimpleApp login')
    noop

login' :: MonadRIO   cfg m
       => HasLogFunc cfg
       => m ()
login' = do
  logDebug "Starting login sequence..."

  putStr "Username: "
  rawUsername <- getLine

  rawPassword <- liftIO . runInputT defaultSettings $ getPassword (Just '•') "Password: "

  logDebug "Attempting verification..."

  -- Validate against

  putStr $ (encodeUtf8 Emoji.okBox) <> " Logged in as " <> rawUsername

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
