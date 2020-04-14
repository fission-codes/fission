module Fission.SemVer.Types (SemVer (..)) where

import qualified RIO.Text as Text

import           Fission.Prelude

-- | Semver broken out by part
data SemVer = SemVer
  { major :: !Word8
  , minor :: !Word8
  , patch :: !Word8
  } deriving Eq

instance Show SemVer where
  show SemVer {..} = show major <> "." <> show minor <> "." <> show patch

instance Display SemVer where
  display SemVer {..} = display major <> "." <> display minor <> "." <> display patch

instance Ord SemVer where
  compare vA vB =
    case (compare (major vA) (major vB), compare (minor vA) (minor vB)) of
      (EQ, EQ)  -> compare (patch vA) (patch vB)
      (EQ, mnr) -> mnr
      (mjr, _)  -> mjr

instance Arbitrary SemVer where
  arbitrary = do
    major <- arbitrary
    minor <- arbitrary
    patch <- arbitrary
    return SemVer {..}

instance ToJSON SemVer where
  toJSON version = String $ textDisplay version

instance FromJSON SemVer where
  parseJSON = withText "SemVer" \txt ->
    case readMaybe . Text.unpack <$> Text.split (== '.') txt of
      [Just major, Just minor, Just patch] -> return SemVer {..}
      _ -> fail $ show txt <> " is not a properly formatted semver"
