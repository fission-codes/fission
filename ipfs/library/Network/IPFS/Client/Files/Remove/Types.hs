module Network.IPFS.Client.Files.Remove.Types (API) where

import           Servant.API
import           Network.IPFS.Prelude


type API
  = "rm"
    :> QueryParam' '[Required] "arg" Text -- path
    :> QueryParam' '[Required] "recursive" Bool
    :> QueryParam' '[] "force" Bool
    :> Post '[JSON] ()
