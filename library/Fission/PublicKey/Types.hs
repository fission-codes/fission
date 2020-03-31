-- FIXME move to Fission.Key.Public
module Fission.PublicKey.Types
  ( PublicKey (..)
  , Algorithm (..)
  ) where

import qualified RIO.Text as Text

import           Data.Swagger
import           Database.Persist.Postgresql

import           Fission.Prelude

-----------------------------------------------------------------------------------

newtype PublicKey = PublicKey { publicKey :: Text }
  deriving (Eq, Show)

instance FromJSON PublicKey where
  parseJSON = withText "PublicKey" (pure . PublicKey)

instance ToJSON PublicKey where
  toJSON (PublicKey pk) = String pk

instance PersistField PublicKey where
  toPersistValue (PublicKey pk) = PersistText pk

  fromPersistValue = \case
    PersistText txt -> Right (PublicKey txt)
    other           -> Left  ("Invalid Persistent DID: " <> Text.pack (show other))

instance PersistFieldSql PublicKey where
  sqlType _pxy = SqlString

-----------------------------------------------------------------------------------

data Algorithm
  = RSA2048
  | Ed25519
  deriving (Eq, Show)

instance PersistField Algorithm where
  toPersistValue = \case
    RSA2048 -> PersistText "RSA2048"
    Ed25519 -> PersistText "Ed25519"

  fromPersistValue = \case
    PersistText "RSA2048" -> Right RSA2048
    PersistText "Ed25519" -> Right Ed25519
    other -> Left ("Invalid Persistent public key algorithm: " <> Text.pack (show other))

instance ToSchema Algorithm where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerString
      |> NamedSchema (Just "PublicKeyAlgorithm")
      |> pure

instance PersistFieldSql Algorithm where
  sqlType _pxy = SqlString
