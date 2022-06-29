module Network.IPFS.Client
  ( API
  , add
  , cat
  , dagPut
  , filesCopy
  , filesRemove
  , filesStat
  , filesWrite
  , pin
  , stat
  , unpin
  ) where

import qualified RIO.ByteString.Lazy                             as Lazy

import           Servant.API
import           Servant.Client
import           Servant.Multipart.Client                        ()

import           Network.IPFS.Internal.Orphanage.ByteString.Lazy ()
import           Network.IPFS.Prelude                            hiding (object)

import           Network.IPFS.CID.Types
import qualified Network.IPFS.File.Form.Types                    as File
import qualified Network.IPFS.File.Types                         as File
import           Network.IPFS.Stat.Types

import qualified Network.IPFS.Client.Add                         as Add
import qualified Network.IPFS.Client.Cat                         as Cat
import qualified Network.IPFS.Client.DAG.Put.Types               as DAG.Put
import qualified Network.IPFS.Client.DAG.Types                   as DAG
import qualified Network.IPFS.Client.Files                       as Files
import qualified Network.IPFS.Client.Files.Statistics.Types      as Files.Statistics
import qualified Network.IPFS.Client.Pin                         as Pin
import qualified Network.IPFS.Client.Stat                        as Stat

type API
  = "api"
  :> "v0"
  :> V0API

type V0API = "add"    :> Add.API
        :<|> "cat"    :> Cat.API
        :<|> "dag"    :> DAG.API
        :<|> "object" :> Stat.API

        :<|> "pin"    :> Pin.API

        :<|> "files"  :> Files.API

add           :: Lazy.ByteString                      -> ClientM CID
cat           :: CID                                  -> ClientM File.Serialized
dagPut        :: Bool -> (Lazy.ByteString, File.Form) -> ClientM DAG.Put.Response
stat          :: CID                                  -> ClientM Stat

pin           :: CID                                  -> ClientM Pin.Response
unpin         :: CID -> Bool                          -> ClientM Pin.Response

filesCopy     :: Text -> Text -> Bool                 -> ClientM ()
filesRemove   :: Text -> Bool -> Maybe Bool           -> ClientM ()
filesStat     :: Text                                 -> ClientM Files.Statistics.Response

filesWrite
  :: Text -> Bool -> Bool -> Bool
  -> Maybe Bool -> Maybe Integer -> Maybe Text
  -> Lazy.ByteString
  -> ClientM ()

add :<|> cat
    :<|> dagPut
    :<|> stat

    :<|> (pin :<|> unpin)
    :<|> (filesCopy :<|> filesRemove :<|> filesStat :<|> filesWrite)

    = client (Proxy @API :: Proxy API)
