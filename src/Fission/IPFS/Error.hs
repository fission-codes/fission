module Fission.IPFS.Error
  ( Add (..)
  , Get (..)
  , Error (..)
  , Linearization (..)
  ) where

import RIO

import Data.Aeson
import Network.HTTP.Types.Status
import Servant.Exception

import Fission.Internal.Orphanage ()
import Fission.IPFS.Types

data Error
  = AddErr Add
  | GetErr Get
  | LinearizationErr Linearization
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance ToServantErr Error where
  status (AddErr           addErr) = status addErr
  status (GetErr           getErr) = status getErr
  status (LinearizationErr linErr) = status linErr

  message (AddErr           addErr) = message addErr
  message (GetErr           getErr) = message getErr
  message (LinearizationErr linErr) = message linErr

data Get
  = InvalidCID Text
  | TimedOut CID Natural
  | UnknownGetErr Text
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance Display Get where
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

    UnknownGetErr raw ->
      "Unknwon IPFS get error: " <> display raw

instance ToServantErr Get where
  status = \case
    InvalidCID _    -> unprocessableEntity422
    TimedOut _ _    -> requestTimeout408
    UnknownGetErr _ -> internalServerError500

  message = \case
    invalid@(InvalidCID _) -> textDisplay invalid
    tOut@(TimedOut _ _)    -> textDisplay tOut
    UnknownGetErr _        -> "Unknown IPFS error"

data Add
  = InvalidFile
  | UnexpectedOutput Text
  | UnknownAddErr Text
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance Display Add where
  display = \case
    InvalidFile          -> "Invalid file"
    UnexpectedOutput txt -> "Unexpected IPFS output: " <> display txt
    UnknownAddErr    txt -> "Unknown IPFS add error: " <> display txt

instance ToServantErr Add where
  status = \case
    InvalidFile        -> status422
    UnknownAddErr    _ -> internalServerError500
    UnexpectedOutput _ -> internalServerError500

  message = \case
    InvalidFile        -> "File not processable by IPFS"
    UnknownAddErr    _ -> "Unknown IPFS error"
    UnexpectedOutput _ -> "Unexpected IPFS result"

-- NOTE Will not stay as a newtype in the long term
newtype Linearization = NonLinear SparseTree
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( Exception
                    , ToJSON
                    )

instance Display Linearization where
  display (NonLinear sparseTree) = "Unable to linearize IPFS result: " <> display sparseTree

instance ToServantErr Linearization where
  status  _ = internalServerError500
  message _ = "Unable to linearize IPFS result"
