module Fission.IPFS.Error
  ( Add (..)
  , Get (..)
  , Error (..)
  , Linearization (..)
  ) where

import Servant.Server

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8
import           Fission.IPFS.Types
import           Fission.Web.Error

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

instance ToServerError Error where
  toServerError = \case
    AddErr           addErr -> toServerError addErr
    GetErr           getErr -> toServerError getErr
    LinearizationErr linErr -> toServerError linErr

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

instance ToServerError Get where
  toServerError = \case
    InvalidCID txt          -> err422 { errBody = UTF8.textToLazyBS txt }
    UnknownGetErr _         -> err500 { errBody = "Unknown IPFS error" }
    (TimedOut (CID hash) _) ->
      ServerError { errHTTPCode     = 408
                  , errReasonPhrase = "Time out"
                  , errBody         = "IPFS timed out looking for " <> UTF8.textToLazyBS hash
                  , errHeaders      = []
                  }

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

instance ToServerError Add where
  toServerError = \case
    InvalidFile        -> err422 { errBody = "File not processable by IPFS" }
    UnknownAddErr    _ -> err500 { errBody = "Unknown IPFS error" }
    UnexpectedOutput _ -> err500 { errBody = "Unexpected IPFS result" }

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

instance ToServerError Linearization where
  toServerError _ = err500 { errBody = "Unable to linearize IPFS result" }
