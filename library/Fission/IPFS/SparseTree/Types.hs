module Fission.IPFS.SparseTree.Types
  ( SparseTree (..)
  , Tag (..)
  ) where

import qualified RIO.HashMap as HashMap
import qualified RIO.Map     as Map
import qualified RIO.Text    as Text

import Data.Swagger hiding (Tag, name)
import Servant

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8
import           Fission.IPFS.CID.Types
import           Fission.IPFS.Name.Types

-- | Directory structure for CIDs and other identifiers
--
-- Examples:
--
-- > Content "abcdef"
--
-- > show $ Directory [(Key "abcdef", Stub "myfile.txt")])]
-- "abcdef/myfile.txt"
data SparseTree
  = Stub Name
  | Content CID
  | Directory (Map Tag SparseTree)
  deriving          ( Eq
                    , Generic
                    , Show
                    )

instance ToSchema SparseTree where
  declareNamedSchema _ =
    mempty
      |> type_       ?~ SwaggerString
      |> description ?~ "A tree of IPFS paths"
      |> example     ?~ toJSON (Directory [(Key "abcdef", Stub "myfile.txt")])
      |> NamedSchema (Just "IPFS Tree")
      |> pure

instance Display (Map Tag SparseTree) where
  display sparseMap =
    "{" <> foldr (\e acc -> e <> ", " <> acc) "}" (prettyKV <$> Map.toList sparseMap)
    where
      prettyKV (k, v) = display k <> " => " <> display v

instance Display SparseTree where
  display = \case
    Stub      name -> display name
    Content   cid  -> display cid
    Directory dir  -> display dir

instance ToJSON SparseTree where
  toJSON = \case
    Stub (Name name)  -> String <| Text.pack name
    Content (CID cid) -> String <| UTF8.stripN 1 cid
    Directory dirMap  -> Object <| HashMap.fromList (jsonKV <$> Map.toList dirMap)
    where
      jsonKV :: (Tag, SparseTree) -> (Text, Value)
      jsonKV (tag, subtree) = (jsonTag tag, toJSON subtree)

      jsonTag (Key (Name n))   = Text.pack n
      jsonTag (Hash (CID cid)) = UTF8.stripN 1 cid

data Tag
  = Key Name
  | Hash CID
  deriving ( Eq
           , Generic
           , Ord
           , Show
           )

instance Display Tag where
  display (Key name) = display name
  display (Hash cid) = display cid

instance FromJSON Tag
instance ToJSON Tag where
  toJSON (Key k)  = toJSON k
  toJSON (Hash h) = toJSON h

instance FromJSONKey Tag
instance ToJSONKey Tag
instance ToSchema Tag

instance FromHttpApiData Tag where
  parseUrlPiece txt = Key <$> parseUrlPiece txt
