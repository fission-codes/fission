-- FIXME move to Fission.Key.Public
module Fission.PublicKey.Types
  ( PublicKey (..)
  , Algorithm (..)
  ) where

import           Data.Binary as Binary
import qualified RIO.Text    as Text

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

instance Binary PublicKey where
  get     = PublicKey . decodeUtf8Lenient <$> Binary.get
  put     = Binary.put . encodeUtf8 . publicKey
  putList = Binary.putList . fmap (encodeUtf8 . publicKey)

instance ToSchema PublicKey where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> description ?~ "A public key"
      |> NamedSchema (Just "PublicKey")
      |> pure

-----------------------------------------------------------------------------------

data Algorithm
  = RSA2048
  | Ed25519
  deriving (Eq, Show)

instance ToJSON Algorithm where
  toJSON = String . \case
    RSA2048 -> "RS256" -- Per the JWT Spec
    Ed25519 -> "Ed25519"

instance FromJSON Algorithm where
  parseJSON = withText "JWT.Algorithm" \case
    "RS256"   -> return RSA2048
    "Ed25519" -> return Ed25519
    other     -> fail (Text.unpack other <> " is not a valid JWT algorithm")
   
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
