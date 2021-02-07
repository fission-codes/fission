module Main (main) where

import           Fission.Prelude

import           Fission.CLI

import           Graphics.UI.Gtk as GTK hiding (Action, backspace)

main :: IO ()
main = do
  void initGUI
  window <- windowNew   -- (2)

  GTK.set window [ windowTitle         := ("Cool Fission GUI" :: Text)]

  GTK.on window objectDestroy mainQuit

  button <- buttonNew
  GTK.set button [ buttonLabel := ("Hello Fission 👋" :: Text) ]
  GTK.set window [ containerChild := button]

  widgetShowAll window  -- (4)
  mainGUI
  -- cli >>= \case
 --   Left  _ -> exitFailure
 --   Right _ -> exitSuccess
