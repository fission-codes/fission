module Fission.CLI where


import RIO
import RIO.ByteString
import RIO.Partial

import qualified Fission.Internal.UTF8 as UTF8

-- import System.FSNotify

import qualified System.Console.ANSI as ANSI

import Options.Applicative as OA
import Options.Applicative.Simple

import Fission.CLI.Types

cli :: CommandM (m ()) -> IO ((), m ())
cli cmds =
  simpleOptions
    "0.0.1"
    "Top lines about what the CLI is for"
    "This CLI does some cool stuff"
    OA.empty
    cmds

commands :: MonadIO m => CommandM (m ())
commands = do
  greet
  print
  exit

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
    "Don't run the printer"
    (const $ putStr "hallo")
    OA.empty

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
        OA.empty

      addCommand
        "later"
        "Exit soon"
        (\_ -> putStr "exiting...\n")
        OA.empty

printer :: IO ()
printer = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  putStr $ "Hello world" <> (encodeUtf8 . UTF8.textShow . fromJust $ unicodeByName "rocket")
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  putStr $ "\nDone" <> rocket <> "\n"
  loading

rocket :: ByteString
rocket = "\xF0\x9F\x9A\x80"

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
