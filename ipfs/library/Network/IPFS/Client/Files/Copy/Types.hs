module Network.IPFS.Client.Files.Copy.Types (API) where

import           Servant.API
import           Network.IPFS.Prelude


type API
  = "cp"
    :> QueryParam' '[Required] "arg" Text -- from
    :> QueryParam' '[Required] "arg" Text -- to
    :> QueryParam' '[Required] "parents" Bool
    :> Post '[JSON] ()
