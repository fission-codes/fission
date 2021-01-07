module Fission.CLI.PIN.Types (PIN (..)) where

import qualified RIO.Text                as Text
import qualified RIO.Vector              as Vector

import           Fission.Prelude

import           Fission.CLI.Digit.Types

data PIN = PIN
  { a :: Digit
  , b :: Digit
  , c :: Digit
  , d :: Digit
  , e :: Digit
  , f :: Digit
  }
  deriving (Show, Eq)

instance Arbitrary PIN where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    pure PIN {..}

instance Display PIN where
  textDisplay PIN {..} = "[" <> meat <> "]"
    where
      meat = Text.intercalate ", " $ fmap textDisplay [a, b, c, d, e, f]

instance ToJSON PIN where
  toJSON PIN {..} = Array $ fmap toJSON [a, b, c, d, e, f]

instance FromJSON PIN where
  parseJSON = withArray "PIN" \vec ->
    case Vector.toList vec of
      [jsonA, jsonB, jsonC, jsonD, jsonE, jsonF] -> do
        a <- parseJSON jsonA
        b <- parseJSON jsonB
        c <- parseJSON jsonC
        d <- parseJSON jsonD
        e <- parseJSON jsonE
        f <- parseJSON jsonF

        pure PIN {..}

      _ ->
        fail $ "Unexpected PIN length: got " <> (show $ Vector.length vec) <> ", but should be 6"
