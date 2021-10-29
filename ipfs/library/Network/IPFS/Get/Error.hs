module Network.IPFS.Get.Error (Error (..)) where

import           Servant.Client

import           Network.IPFS.Prelude

import           Network.IPFS.Stat.Error
import           Network.IPFS.Types

data Error
  = InvalidCID Text
  | TimedOut CID Natural
  | WebError ClientError
  | SizeError OverflowDetected
  | UnexpectedOutput Text
  | UnknownErr Text
  deriving ( Exception
           , Eq
           , Generic
           , Show
           )

instance Display Error where
  display = \case
    InvalidCID hash ->
      "Invalid CID: " <> display hash

    TimedOut (CID hash) sec ->
      mconcat
        [ "Unable to find CID "
        , display hash
        , " before the timeout of "
        , display sec
        , " seconds."
        ]

    WebError err ->
      "WebError: " <> displayShow err

    SizeError err ->
      "SizeError: " <> display err

    UnexpectedOutput raw ->
      "Unexpected IPFS output: " <> display raw

    UnknownErr raw ->
      "Unknown IPFS get error: " <> display raw
