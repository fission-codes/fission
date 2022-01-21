{-# LANGUAGE DerivingVia #-}
module Test.Web.UCAN.Example
  ( Resource(..)
  , Potency(..)
  ) where

import qualified RIO.Text              as Text
import           Test.Prelude

import           Web.UCAN.Proof.Class

data Resource
  = OnlyOneThing
  | OnlySome
  | Everything
  deriving (Show, Eq, Ord)
  deriving DelegationSemantics
    via (GreaterDelegatesMore Resource)

data Potency
  = CanLook
  | CanTouch
  | SuperUser
  deriving (Show, Eq)

-- You can either look or touch. You can only do both when you're SuperUser.
instance DelegationSemantics Potency where
  SuperUser `canDelegate` _       = True
  _ `canDelegate` SuperUser       = False
  CanLook `canDelegate` CanLook   = True
  CanLook `canDelegate` _         = False
  CanTouch `canDelegate` CanTouch = True
  CanTouch `canDelegate` _        = False

instance Arbitrary Resource where
  arbitrary = elements [OnlyOneThing, OnlySome, Everything]

instance Arbitrary Potency where
  arbitrary = elements [CanLook, CanTouch, SuperUser]

instance FromJSON Resource where
  parseJSON str  = str & withText "AuthZ.Resource" \txt ->
    case txt of
      "OnlyOneThing" -> pure OnlyOneThing
      "OnlySome"     -> pure OnlySome
      "Everything"   -> pure Everything
      nope -> fail $ show nope <> " is not a valid authorization resource"

instance ToJSON Resource where
  toJSON = \case
    OnlyOneThing -> String "OnlyOneThing"
    OnlySome     -> String "OnlySome"
    Everything   -> String "Everything"

instance FromJSON Potency where
  parseJSON str  = str & withText "AuthZ.Potency" \txt ->
    case Text.toUpper txt of
      "CAN_LOOK"   -> pure CanLook
      "CAN_TOUCH"  -> pure CanTouch
      "SUPER_USER" -> pure SuperUser
      nope -> fail $ show nope <> " is not a valid authorization potency"

instance ToJSON Potency where
  toJSON = \case
    CanLook   -> String "CAN_LOOK"
    CanTouch  -> String "CAN_TOUCH"
    SuperUser -> String "SUPER_USER"
