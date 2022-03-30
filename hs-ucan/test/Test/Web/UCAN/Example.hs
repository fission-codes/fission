{-# LANGUAGE DerivingVia #-}
module Test.Web.UCAN.Example
  ( Resource(..)
  , Ability(..)
  ) where

import qualified RIO.Text             as Text
import           Test.Web.UCAN.Prelude

import           Web.UCAN.Proof.Class


data Resource
  = OnlyOneThing
  | OnlySome
  | Everything
  deriving (Show, Eq, Ord)
  deriving DelegationSemantics
    via (GreaterDelegatesMore Resource)

data Ability
  = CanLook
  | CanTouch
  | SuperUser
  deriving (Show, Eq)

-- You can either look or touch. You can only do both when you're SuperUser.
instance DelegationSemantics Ability where
  SuperUser `canDelegate` _         = True
  _         `canDelegate` SuperUser = False
  CanLook   `canDelegate` CanLook   = True
  CanLook   `canDelegate` _         = False
  CanTouch  `canDelegate` CanTouch  = True
  CanTouch  `canDelegate` _         = False

instance Arbitrary Resource where
  arbitrary = elements [OnlyOneThing, OnlySome, Everything]

instance Arbitrary Ability where
  arbitrary = elements [CanLook, CanTouch, SuperUser]

instance FromJSON Resource where
  parseJSON = withText "AuthZ.Resource" \case
    "OnlyOneThing" -> pure OnlyOneThing
    "OnlySome"     -> pure OnlySome
    "Everything"   -> pure Everything
    nope -> fail $ show nope <> " is not a valid authorization resource"

instance Display Resource where
  textDisplay OnlyOneThing = "OnlyOneThing"
  textDisplay OnlySome     = "OnlySome"
  textDisplay Everything   = "Everything"

instance FromJSON Ability where
  parseJSON = withText "AuthZ.Ability" \txt ->
    case Text.toUpper txt of
      "CAN_LOOK"   -> pure CanLook
      "CAN_TOUCH"  -> pure CanTouch
      "SUPER_USER" -> pure SuperUser
      nope -> fail $ show nope <> " is not a valid authorization potency"

instance Display Ability where
  textDisplay CanLook   = "CAN_LOOK"
  textDisplay CanTouch  = "CAN_TOUCH"
  textDisplay SuperUser = "SUPER_USER"
