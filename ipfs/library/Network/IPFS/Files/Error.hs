module Network.IPFS.Files.Error (Error (..)) where

import           Network.IPFS.Prelude
import           Network.IPFS.Types

data Error
  = DestinationAlreadyExists
  | InvalidCID Text
  | IPFSDaemonErr Text
  | UnexpectedOutput Text
  | UnknownFilesErr Text
  | TimedOutCopy Natural
  | TimedOutStat Natural
  | TimedOutWrite Natural
  deriving ( Exception
           , Eq
           , Generic
           , Show
           )

instance Display Error where
  display = \case
    DestinationAlreadyExists ->
      "Cannot copy to destination, item already exists"

    InvalidCID hash ->
      "Invalid CID: " <> display hash

    IPFSDaemonErr txt ->
      "IPFS Daemon error: " <> display txt

    TimedOutCopy sec ->
      mconcat
        [ "Unable to copy before the timeout of "
        , display sec
        , " seconds."
        ]

    TimedOutStat sec ->
      mconcat
        [ "Unable to get statistics before the timeout of "
        , display sec
        , " seconds."
        ]

    TimedOutWrite sec ->
      mconcat
        [ "Unable to write before the timeout of "
        , display sec
        , " seconds."
        ]

    UnexpectedOutput txt ->
      "Unexpected IPFS output: " <> display txt

    UnknownFilesErr txt ->
      "Unknown IPFS files error: " <> display txt
