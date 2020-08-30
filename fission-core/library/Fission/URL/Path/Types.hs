module Fission.URL.Path.Types (Path (..)) where

import qualified RIO.Text as Text

import           Fission.Prelude

data Path a = a `WithPath` [Text]
  deriving (Eq, Show)

instance Display a => Display (Path a) where
  textDisplay (a `WithPath` segments) = textDisplay a <> "/" <> path
    where
      path :: Text
      path = Text.intercalate "/" segments
