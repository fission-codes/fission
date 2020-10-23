-- | Scoped resource(s) that permits access as a bearer proof

module Fission.Authorization.Access.Types (Access (..)) where

import qualified RIO.Text        as Text

import           Fission.Prelude

import           Fission.Models

data Access privilege
  = AllBelongingTo (Entity User)
  | Specifically   privilege
  deriving (Show, Eq)

instance Show privilege => Display (Access privilege) where
  textDisplay = Text.pack . show
