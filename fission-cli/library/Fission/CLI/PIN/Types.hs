module Fission.CLI.PIN.Types (PIN (..)) where

import qualified RIO.Text                as Text
import qualified RIO.Vector              as Vector

import           Servant.API

import           Fission.Prelude

import           Fission.Emoji.Class
import           Fission.Error.Types

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

instance ToEmoji PIN where
  toEmoji PIN {..} = toEmoji ([a, b, c, d, e, f] :: [Digit])

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

instance Display (Mismatch PIN) where
  display _ = "PIN codes do not match"

instance MimeRender OctetStream PIN where
  mimeRender _ pin =  encode pin

instance MimeUnrender OctetStream PIN where
  mimeUnrender _ lbs = eitherDecode lbs
