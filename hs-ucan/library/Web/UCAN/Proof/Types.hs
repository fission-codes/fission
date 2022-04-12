module Web.UCAN.Proof.Types
  ( Proof(..)
  ) where

import           Data.Aeson
import           Network.IPFS.CID.Types

import           RIO                                           hiding (exp)
import qualified RIO.Text                                      as Text
import           Web.UCAN.Internal.Orphanage.Ed25519.SecretKey ()



data Proof
  = Nested    Text
  | Reference CID
  deriving (Show, Eq)


instance Display Proof where
  display = \case
    Nested raw    -> "Nested "    <> display raw
    Reference cid -> "Reference " <> display cid

instance ToJSON Proof where
  toJSON = \case
    Reference cid ->
      toJSON cid

    Nested raw ->
      String raw

instance FromJSON Proof where
  parseJSON val =
    val
      & withText "Credential Proof" \txt ->
        if "eyJ" `Text.isPrefixOf` txt -- i.e. starts with Base64 encoded '{'
          then pure $ Nested txt
          else Reference <$> parseJSON val
