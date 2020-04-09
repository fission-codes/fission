module Fission.Key.Asymmetric.Public.Types
  ( Public (..)
  , Algorithm (..)
  ) where

import           Data.Binary    as Binary
import           Data.ByteArray

import qualified RIO.Text as Text hiding (length)

import           Data.Swagger
import           Database.Persist.Postgresql

import           Fission.Prelude hiding (length)
import           Fission.Key.Asymmetric.Algorithm.Types

newtype Public = Public { publicKey :: ByteString }
  deriving (Eq, Show)

instance Arbitrary Public where
  arbitrary = Public <$> arbitrary

instance FromJSON Public where
  parseJSON = withText "PublicKey" (pure . Public . encodeUtf8)

instance ToJSON Public where
  toJSON (Public pk) = String $ decodeUtf8Lenient pk

instance PersistField Public where
  toPersistValue (Public pk) = PersistText $ decodeUtf8Lenient pk

  fromPersistValue = \case
    PersistText txt -> Right (Public $ encodeUtf8 txt)
    other           -> Left  ("Invalid Persistent DID: " <> Text.pack (show other))

instance PersistFieldSql Public where
  sqlType _pxy = SqlString

instance Binary Public where
  get     = Public <$> Binary.get
  put     = Binary.put . publicKey
  putList = Binary.putList . fmap publicKey

instance ToSchema Public where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> description ?~ "A public key"
      |> NamedSchema (Just "PublicKey")
      |> pure

instance ByteArrayAccess Public where
  length        (Public bs) = length bs
  withByteArray (Public bs) = withByteArray bs
