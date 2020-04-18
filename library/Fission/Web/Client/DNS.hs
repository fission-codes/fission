module Fission.Web.Client.DNS (update) where

import           Fission.Prelude
import qualified Fission.URL.DomainName.Types as URL

update :: Proxy URL.DomainName
update = Proxy
