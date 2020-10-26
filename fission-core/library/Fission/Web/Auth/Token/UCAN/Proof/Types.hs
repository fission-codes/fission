{-# LANGUAGE UndecidableInstances #-}

module Fission.Web.Auth.Token.UCAN.Proof.Types
  ( Proof (..)
  , DelegateProof (..)
  ) where

import           Network.IPFS.CID.Types

import qualified RIO.Text                                    as Text

import           Fission.Prelude

import qualified Fission.Web.Auth.Token.JWT.RawContent       as JWT
import           Fission.Web.Auth.Token.JWT.RawContent.Class
import           Fission.Web.Auth.Token.JWT.Signature        as Signature

data Proof ucan
  = RootCredential -- ^ i.e. "Self evident"
  | DelegatedFrom (NonEmpty (DelegateProof ucan))
  deriving (Show, Eq)

-- FIXME move to Proof.Delegate

data DelegateProof ucan
  = Nested    JWT.RawContent ucan
  | Reference CID
  deriving (Show, Eq)

instance (ToRawContent ucan, Arbitrary ucan) => Arbitrary (Proof ucan) where
  arbitrary = frequency
    [ (1, DelegatedFrom <$> nested)
    , (5, pure RootCredential)
    ]
    where
      nested :: Gen (NonEmpty (DelegateProof ucan))
      nested = do
        innerUCAN <- arbitrary
        return (Nested (toRawContent innerUCAN) innerUCAN :| [])

instance
  ( HasField' "sig" ucan Signature
  , ToJSON ucan
  )
  => ToJSON (Proof ucan) where
    toJSON RootCredential        = Null
    toJSON (DelegatedFrom inner) = toJSON inner

instance
  ( HasField' "sig" ucan Signature
  , ToJSON ucan
  )
  => ToJSON (DelegateProof ucan) where
    toJSON = \case
      Reference cid ->
        toJSON cid

      Nested (JWT.RawContent raw) ucan ->
        String (raw <> "." <> textDisplay (getField @"sig" ucan))

instance FromJSON ucan => FromJSON (Proof ucan) where
  parseJSON Null = return RootCredential
  parseJSON val  = DelegatedFrom <$> parseJSON val

instance FromJSON ucan => FromJSON (DelegateProof ucan) where
  parseJSON val = withText "DelegateProof" resolver val
    where
      resolver txt =
        if "eyJ" `Text.isPrefixOf` txt -- i.e. starts with Base64 encoded '{'
          then Nested (JWT.contentOf txt) <$> parseJSON val
          else Reference <$> parseJSON val
