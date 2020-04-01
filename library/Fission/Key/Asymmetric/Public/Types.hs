module Fission.Key.Asymmetric.Public.Types
  ( Public (..)
  , Algorithm (..)
  ) where

import           Data.Binary as Binary
import qualified RIO.Text    as Text

import           Data.Swagger
import           Database.Persist.Postgresql

import           Fission.Prelude
import           Fission.Key.Asymmetric.Algorithm.Types

newtype Public = Public { publicKey :: Text }
  deriving (Eq, Show)

instance FromJSON Public where
  parseJSON = withText "PublicKey" (pure . Public)

instance ToJSON Public where
  toJSON (Public pk) = String pk

instance PersistField Public where
  toPersistValue (Public pk) = PersistText pk

  fromPersistValue = \case
    PersistText txt -> Right (Public txt)
    other           -> Left  ("Invalid Persistent DID: " <> Text.pack (show other))

instance PersistFieldSql Public where
  sqlType _pxy = SqlString

instance Binary Public where
  get     = Public . decodeUtf8Lenient <$> Binary.get
  put     = Binary.put . encodeUtf8 . publicKey
  putList = Binary.putList . fmap (encodeUtf8 . publicKey)

instance ToSchema Public where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> description ?~ "A public key"
      |> NamedSchema (Just "PublicKey")
      |> pure
