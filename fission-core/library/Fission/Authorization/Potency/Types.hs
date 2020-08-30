module Fission.Authorization.Potency.Types (Potency (..)) where

import qualified RIO.Text as Text

import           Fission.Prelude

{-
Maybe appending to the private side requires update permissions?
Since we can't validate the actual internal paths, you'd need at least
append-only rights, so
-}

-- | How much power allowed in an authorization
data Potency
  = AuthNOnly   -- ^ Read signature only -- just a proof. Cannot delegate further.
  | AppendOnly  -- ^ Append new files
  | Destructive -- ^ Overwrite / destroy. "Ownership is the right to destroy"
  | SuperUser   -- ^ i.e. SuperUser -- Financial, major account settings, &c
  deriving (Show, Eq, Ord)

instance Display Potency where
  textDisplay = Text.pack . show

instance Arbitrary Potency where
  arbitrary = elements [AuthNOnly, AppendOnly, Destructive, SuperUser]

instance ToJSON Potency where
  toJSON = \case
    AuthNOnly   -> Null
    AppendOnly  -> String "APPEND"
    Destructive -> String "DESTROY"
    SuperUser   -> String "SUPER_USER"

instance FromJSON Potency where
  parseJSON Null = pure AuthNOnly
  parseJSON str  = str |> withText "AuthZ.Potency" \txt ->
    case Text.toUpper txt of
      "APPEND"     -> pure AppendOnly
      "DESTROY"    -> pure Destructive
      "SUPER_USER" -> pure SuperUser
      nope -> fail $ show nope <> " is not a valid authorization potency"
