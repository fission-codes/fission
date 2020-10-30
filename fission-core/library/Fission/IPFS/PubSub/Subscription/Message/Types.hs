module Fission.IPFS.PubSub.Subscription.Message.Types (Message (..)) where

import qualified Data.ByteString.Base64 as BS64

import           Fission.Prelude

data Message a = Message
  { sender   :: Text
  , seqNoB64 :: ByteString
  , topicIDs :: [Text]
  , payload  :: a
  }
  deriving (Show, Eq)

instance ToJSON a => ToJSON (Message a) where
  toJSON Message {..} =
    object [ "from"     .= sender
           , "seqno"    .= decodeUtf8Lenient seqNoB64
           , "topicIDs" .= topicIDs
           , "data"     .= payload
           ]

instance FromJSON a => FromJSON (Message a) where
  parseJSON = withObject "IPFS.SubMessage" \obj -> do
    sender       <- obj .: "from"
    topicIDs     <- obj .: "topicIDs"
    seqNoTxt     <- obj .: "seqno"
    payloadTxt64 <- obj .: "data"

    let
      seqNoB64   = encodeUtf8 seqNoTxt
      payloadTxt = decodeUtf8Lenient . BS64.decodeLenient $ encodeUtf8 payloadTxt64

    payload <- withEmbeddedJSON "payload" parseJSON (String payloadTxt)

    return Message {..}
