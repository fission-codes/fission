module Network.IPFS.Client
  ( API
  , add
  , cat
  , stat
  , pin
  , unpin
  , dagPut
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
import qualified Network.IPFS.Client.Pin                         as Pin
import qualified Network.IPFS.Client.Stat                        as Stat

type API
  = "api"
  :> "v0"
  :> V0API

type V0API = "add"    :> Add.API
        :<|> "cat"    :> Cat.API
        :<|> "object" :> Stat.API
        :<|> "dag"    :> DAG.API
        :<|> "pin"    :> Pin.API

cat    :: CID                                         -> ClientM File.Serialized
stat   :: CID                                         -> ClientM Stat
pin    :: CID                                         -> ClientM Pin.Response
unpin  :: CID -> Bool                                 -> ClientM Pin.Response
dagPut ::        Bool -> (Lazy.ByteString, File.Form) -> ClientM DAG.Put.Response
add    ::                 Lazy.ByteString             -> ClientM CID

add :<|> cat
    :<|> stat
    :<|> dagPut
    :<|> pin
    :<|> unpin = client $ Proxy @API
