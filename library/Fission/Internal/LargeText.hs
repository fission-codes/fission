module Fission.Internal.LargeText (LargeText(..)) where

import           Fission.Prelude

import qualified Fission.DNS          as DNS
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified RIO.NonEmpty         as NonEmpty
import qualified RIO.Text as Text


newtype LargeText = LargeText Text
  deriving newtype (Show, Eq)

instance Arbitrary LargeText where
  arbitrary = do
    len <- arbitrary
    txt <- txtMinLen "" $ 257 + abs len
    return $ LargeText txt

    where
      txtMinLen acc len =
        case Text.compareLength acc len of
          LT -> do
            newTxt <- arbitrary
            txtMinLen (acc <> newTxt) len

          _ -> 
            return acc
