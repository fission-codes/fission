module Network.IPFS.Client.Files.Write.Types (API) where

import           Servant.API

import qualified RIO.ByteString.Lazy as Lazy

import           Network.IPFS.Prelude


type API
  = "write"
    :> QueryParam' '[Required] "arg" Text         -- path
    :> QueryParam' '[Required] "create" Bool
    :> QueryParam' '[Required] "parents" Bool
    :> QueryParam' '[Required] "truncate" Bool
    :> QueryParam' '[] "raw-leaves" Bool          -- (experimental) Use raw blocks for newly created leaf nodes.
    :> QueryParam' '[] "cid-version" Integer      -- (experimental) Cid version to use.
    :> QueryParam' '[] "hash" Text                -- (experimental) Hash function to use.
    :> ReqBody '[OctetStream] Lazy.ByteString
    :> Post '[JSON] ()
