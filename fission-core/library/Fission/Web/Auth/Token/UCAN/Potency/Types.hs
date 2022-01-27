module Fission.Web.Auth.Token.UCAN.Potency.Types (Potency (..)) where

import           RIO
import qualified RIO.Text             as Text

import           Data.Aeson
import           Test.QuickCheck

import           Web.UCAN.Proof.Class

{-
Maybe appending to the private side requires update permissions?
Since we can't validate the actual internal paths, you'd need at least
append-only rights, so
-}

-- | How much power allowed in an authorization
data Potency
  = AppendOnly  -- ^ Append new files
  | Destructive -- ^ Overwrite / destroy. "Ownership is the right to destroy"
  | SuperUser   -- ^ i.e. SuperUser -- Financial, major account settings, &c
  deriving (Show, Eq, Enum)

instance Display (Maybe Potency) where
  textDisplay = Text.pack . show

instance Arbitrary Potency where
  arbitrary = elements [AppendOnly, Destructive, SuperUser]

instance ToJSON Potency where
  toJSON = \case
    AppendOnly  -> String "APPEND"
    Destructive -> String "DESTROY"
    SuperUser   -> String "SUPER_USER"

instance FromJSON Potency where
  parseJSON str  = str & withText "AuthZ.Potency" \txt ->
    case Text.toUpper txt of
      "APPEND"     -> pure AppendOnly
      "DESTROY"    -> pure Destructive
      "SUPER_USER" -> pure SuperUser
      nope -> fail $ show nope <> " is not a valid authorization potency"

instance DelegationSemantics Potency where
  proofPtc `canDelegate` ptc = fromEnum proofPtc >= fromEnum ptc
