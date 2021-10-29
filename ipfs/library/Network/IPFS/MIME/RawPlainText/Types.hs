module Network.IPFS.MIME.RawPlainText.Types (RawPlainText) where

import           Network.HTTP.Media
import qualified Servant.API        as API

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

-- Built-in version includes charset
-- https://github.com/haskell-servant/servant/issues/1002
data RawPlainText

instance API.Accept RawPlainText where
  contentType _ = "text" // "plain"

instance API.MimeRender RawPlainText Text where
  mimeRender _ = Lazy.fromStrict . encodeUtf8

instance API.MimeRender RawPlainText ByteString where
  mimeRender _ = Lazy.fromStrict

instance API.MimeRender RawPlainText Lazy.ByteString where
  mimeRender _ = id

instance API.MimeUnrender RawPlainText Text where
  mimeUnrender _ bs =
    case decodeUtf8' $ Lazy.toStrict bs of
      Left  err -> Left $ show err
      Right txt -> Right txt

instance API.MimeUnrender RawPlainText ByteString where
  mimeUnrender _ = Right . Lazy.toStrict

instance API.MimeUnrender RawPlainText Lazy.ByteString where
  mimeUnrender _ = Right
