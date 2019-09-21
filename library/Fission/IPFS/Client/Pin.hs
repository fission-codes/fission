module Fission.IPFS.Client.Pin
  ( API
  , AddAPI
  , RemoveAPI
  , Response (..)
  ) where

import RIO

import Data.Aeson
import Servant

import           Fission.IPFS.CID.Types
import qualified Fission.IPFS.Client.Param as Param

type API = AddAPI :<|> RemoveAPI

type AddAPI
  = "add"
    :> Param.CID
    :> Put '[JSON] Response

type RemoveAPI
  = "rm"
    :> Param.CID
    :> Param.IsRecursive
    :> Delete '[JSON] Response

newtype Response = Response { cids :: [CID] }
  deriving (Eq, Show)

instance FromJSON Response where
  parseJSON = withObject "Pin Response" \obj ->
    Response <$> obj .: "Pins"
