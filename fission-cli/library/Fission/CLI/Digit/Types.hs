module Fission.CLI.Digit.Types (Digit (..)) where

import           Data.Scientific
import           Prelude         (Enum (..))

import           Fission.Prelude hiding (Enum)

data Digit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Show, Eq, Ord)

instance Arbitrary Digit where
  arbitrary = toEnum <$> elements [0..9]

instance Enum Digit where
  toEnum = \case
    0 -> Zero
    1 -> One
    2 -> Two
    3 -> Three
    4 -> Four
    5 -> Five
    6 -> Six
    7 -> Seven
    8 -> Eight
    9 -> Nine
    _ -> error "not a valid Digit"

  fromEnum = \case
    Zero  -> 0
    One   -> 1
    Two   -> 2
    Three -> 3
    Four  -> 4
    Five  -> 5
    Six   -> 6
    Seven -> 7
    Eight -> 8
    Nine  -> 9

instance Display Digit where
  display = display . fromEnum

instance ToJSON Digit where
  toJSON = Number . fromIntegral . fromEnum

instance FromJSON Digit where
  parseJSON = withScientific "Digit" \sciNum ->
    case floatingOrInteger sciNum of
      Left (_ :: Double) ->
        fail $ "Non-integers are not valid digits: " <> show sciNum

      Right int ->
        if int >= 0 && int <= 10
          then pure $ toEnum int
          else fail "PIN elements must be single digits"
