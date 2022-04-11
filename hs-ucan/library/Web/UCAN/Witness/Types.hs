module Web.UCAN.Witness.Types
  ( Witness(..)
  ) where

import           Data.Aeson
import           Network.IPFS.CID.Types

import           RIO                                           hiding (exp)
import qualified RIO.Text                                      as Text
import           Web.UCAN.Internal.Orphanage.Ed25519.SecretKey ()



data Witness
  = Nested    Text
  | Reference CID
  deriving (Show, Eq)


instance Display Witness where
  display = \case
    Nested raw    -> "Nested "    <> display raw
    Reference cid -> "Reference " <> display cid

instance ToJSON Witness where
  toJSON = \case
    Reference cid ->
      toJSON cid

    Nested raw ->
      String raw

instance FromJSON Witness where
  parseJSON val =
    val
      & withText "Credential Witness" \txt ->
        if "eyJ" `Text.isPrefixOf` txt -- i.e. starts with Base64 encoded '{'
          then pure $ Nested txt
          else Reference <$> parseJSON val
