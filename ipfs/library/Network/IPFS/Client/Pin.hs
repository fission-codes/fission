module Network.IPFS.Client.Pin
  ( API
  , AddAPI
  , RemoveAPI
  , Response (..)
  ) where

import qualified RIO.Text                  as Text

import           Servant.API

import           Network.IPFS.Prelude

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Client.Param as Param

type API = AddAPI :<|> RemoveAPI

type AddAPI
  = "add"
    :> Param.CID'
    :> Post '[JSON] Response

-- IPFS v0.5 disallows GET requests
-- https://docs.ipfs.io/recent-releases/go-ipfs-0-5/#breaking-changes-upgrade-notes
type RemoveAPI
  = "rm"
    :> Param.CID'
    :> Param.IsRecursive
    :> Post '[JSON] Response

newtype Response = Response { cids :: [CID] }
  deriving (Eq, Show)

instance Display Response where
  textDisplay Response {cids} = "[" <> inner <> "]"
    where
      inner = Text.intercalate ", " $ fmap textDisplay cids

instance FromJSON Response where
  parseJSON = withObject "Pin Response" \obj ->
    Response <$> obj .: "Pins"
