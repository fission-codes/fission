{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Web.UCAN.Example
  ( Resource(..)
  , Ability(..)
  , PathResource(..)
  , EmailResource(..)
  , MsgAbility(..)
  ) where

import           Data.Coerce
import           Data.Monoid
import qualified RIO.List                    as List
import qualified RIO.Text                    as Text
import           Test.Web.UCAN.Prelude

import qualified Text.URI                    as URI
import qualified Text.URI.QQ                 as URI
import           Web.UCAN.Capabilities.Class
import           Web.UCAN.Proof.Class


----------------------
-- Limited & Simple --
----------------------


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
  deriving (Show, Eq, Ord)

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

instance IsResource Resource where
  parseResourceURI = \case
    URI.URI
      { uriScheme = Just (URI.unRText -> "example")
      , uriPath = Just (False, thing :| [])
      } -> case URI.unRText thing of
        "OnlyOneThing" -> pure OnlyOneThing
        "OnlySome"     -> pure OnlySome
        "Everything"   -> pure Everything
        _              -> fail $ Text.unpack (URI.unRText thing) <> " is not a valid example resource path"

    nope ->
      fail $ show nope <> " is not a valid example resource"

  renderResourceURI res = URI.URI
    { uriScheme = Just [URI.scheme|example|]
    , uriAuthority = Left False
    , uriPath = Just (False, path :| [])
    , uriQuery = []
    , uriFragment = Nothing
    }
    where
      path = case res of
        OnlyOneThing -> [URI.pathPiece|OnlyOneThing|]
        OnlySome     -> [URI.pathPiece|OnlySome|]
        Everything   -> [URI.pathPiece|Everything|]


instance IsAbility Ability where
  parseAbilityParts ("example", "can_look")   = Just CanLook
  parseAbilityParts ("example", "can_touch")  = Just CanTouch
  parseAbilityParts ("example", "super_user") = Just SuperUser
  parseAbilityParts _                         = Nothing

  renderAbilityParts CanLook   = ("example", "can_look")
  renderAbilityParts CanTouch  = ("example", "can_touch")
  renderAbilityParts SuperUser = ("example", "super_user")

------------------
-- PathResource --
------------------

data PathResource
  = PathResource [URI.RText 'URI.PathPiece] deriving (Show, Eq, Ord)

instance Arbitrary PathResource where
  arbitrary = do
    let exampleSegments =
          [ [URI.pathPiece|public|]
          , [URI.pathPiece|private|]
          , [URI.pathPiece|test|]
          , [URI.pathPiece|Apps|]
          , [URI.pathPiece|Docs|]
          ]
    len <- chooseInt (0, 5)
    segments <- sequence $ List.replicate len $ elements exampleSegments
    return $ PathResource segments

instance DelegationSemantics PathResource where
  (PathResource parentPath) `canDelegate` (PathResource childPath) =
    length parentPath <= length childPath
      && (coerce @([All] -> All) mconcat) (zipWith (==) parentPath childPath)

instance IsResource PathResource where
  parseResourceURI = \case
    URI.URI
      { uriScheme = Just (URI.unRText -> "path")
      , uriAuthority = Left True
      , uriPath = Nothing
      , uriQuery = []
      , uriFragment = Nothing
      } ->
        Just $ PathResource []

    URI.URI
      { uriScheme = Just (URI.unRText -> "path")
      , uriAuthority = Left True
      , uriPath = Just (False, head :| rest)
      , uriQuery = []
      , uriFragment = Nothing
      } ->
        Just $ PathResource (head:rest)

    _ ->
      Nothing

  renderResourceURI (PathResource []) = URI.URI
    { uriScheme = Just [URI.scheme|path|]
    , uriAuthority = Left True
    , uriPath = Nothing
    , uriQuery = []
    , uriFragment = Nothing
    }
  renderResourceURI (PathResource (head:rest)) = URI.URI
    { uriScheme = Just [URI.scheme|path|]
    , uriAuthority = Left True
    , uriPath = Just (False, head :| rest)
    , uriQuery = []
    , uriFragment = Nothing
    }


----------------------
-- Email Capability --
----------------------

data EmailResource
  = EmailResource (URI.RText 'URI.Username) (URI.RText 'URI.Host)
  deriving (Show, Eq, Ord)
  deriving DelegationSemantics via (EqualCanDelegate EmailResource)

instance IsResource EmailResource where
  parseResourceURI = \case
    URI.URI
      { uriScheme = Just (URI.unRText -> "email")
      , uriAuthority = Right
          (URI.Authority
            { authUserInfo = Just
                (URI.UserInfo
                  { uiUsername = username
                  , uiPassword = Nothing
                  }
                )
            , authHost = host
            , authPort = Nothing
            }
          )
      , uriPath = Nothing
      , uriQuery = []
      , uriFragment = Nothing
      } ->
        Just $ EmailResource username host

    _ ->
      Nothing

  renderResourceURI (EmailResource username host) = URI.URI
    { uriScheme = Just [URI.scheme|email|]
    , uriAuthority = Right $ URI.Authority
      { authUserInfo = Just $ URI.UserInfo { uiUsername = username, uiPassword = Nothing }
      , authHost = host
      , authPort = Nothing
      }
    , uriPath = Nothing
    , uriQuery = []
    , uriFragment = Nothing
    }

data MsgAbility = MsgSend deriving (Show, Eq, Ord)
  deriving DelegationSemantics via (EqualCanDelegate MsgAbility)

instance IsAbility MsgAbility where
  parseAbilityParts ("msg", "send") = Just MsgSend
  parseAbilityParts _               = Nothing

  renderAbilityParts MsgSend = ("msg", "send")
