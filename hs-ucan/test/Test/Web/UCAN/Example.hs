{-# LANGUAGE DerivingVia #-}
module Test.Web.UCAN.Example
  ( Capability(..)
  , Resource(..)
  , Potency(..)
  ) where

import qualified RIO.Text               as Text
import           Test.Prelude

import           Web.UCAN.Witness.Class


data Capability
  = Capability
  { resource :: Resource
  , potency  :: Potency
  }
  deriving (Show, Eq)

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

instance DelegationSemantics Capability where
  a `canDelegate` b =
      resource a `canDelegate` resource b
    && potency a `canDelegate` potency b

-- You can either look or touch. You can only do both when you're SuperUser.
instance DelegationSemantics Potency where
  SuperUser `canDelegate` _         = True
  _         `canDelegate` SuperUser = False
  CanLook   `canDelegate` CanLook   = True
  CanLook   `canDelegate` _         = False
  CanTouch  `canDelegate` CanTouch  = True
  CanTouch  `canDelegate` _         = False

instance Arbitrary Capability where
  arbitrary = Capability <$> arbitrary <*> arbitrary

instance Arbitrary Resource where
  arbitrary = elements [OnlyOneThing, OnlySome, Everything]

instance Arbitrary Potency where
  arbitrary = elements [CanLook, CanTouch, SuperUser]

instance FromJSON Capability where
  parseJSON = withObject "AuthZ.Capability" \obj -> do
    resource <- obj .: "rsc"
    potency  <- obj .: "pot"
    return Capability {..}

instance ToJSON Capability where
  toJSON Capability {..} = object
    [ "rsc" .= resource
    , "pot" .= potency
    ]

instance FromJSON Resource where
  parseJSON = withText "AuthZ.Resource" \case
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
  parseJSON = withText "AuthZ.Potency" \txt ->
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
