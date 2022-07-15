module Fission.FileSystem.DirectoryName.Types (DirectoryName (..)) where

import           RIO.Text                   as Text

import           Data.Swagger
import           Servant.API

import           Fission.Prelude

import           Fission.URL.Validation

import           Fission.FileSystem.DirectoryName.Error

newtype DirectoryName = DirectoryName { directoryName :: Text }
  deriving          ( Generic )
  deriving anyclass ( ToSchema
                    , ToParamSchema
                    )
  deriving newtype  ( Eq
                    , Show
                    , IsString
                    )

mkDirectoryName :: Text -> Either Invalid DirectoryName
mkDirectoryName txt =
  if isValid normalized
    then Right $ DirectoryName normalized
    else Left Invalid

  where
    normalized = Text.toLower txt

instance Arbitrary DirectoryName where
  arbitrary = do
    txt <- arbitrary
    case mkDirectoryName $ Text.filter isURLChar txt of
      Left _      -> arbitrary
      Right dName -> return dName

instance FromHttpApiData DirectoryName where
  parseUrlPiece = Right . DirectoryName

instance ToHttpApiData DirectoryName where
  toUrlPiece (DirectoryName directoryName) = directoryName
