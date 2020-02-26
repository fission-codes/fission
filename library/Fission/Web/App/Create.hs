module Fission.Web.App.Create
  ( API
  , create
  , splashCID
  ) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models
import           Fission.URL

import qualified Fission.App.Creator.Class  as App
import           Fission.IPFS.DNSLink.Class as DNSLink
import           Fission.Web.Error

type API = PostAccepted '[JSON] (Subdomain, DomainName)

create ::
  ( MonadTime      m
  , MonadDNSLink   m
  , MonadDB      t m
  , App.Creator  t
  )
  => Entity User
  -> ServerT API m
create (Entity userId _) =
  splashCID
    |> App.create userId
    |> runDBNow
    |> bind \case
      Right (_, subdomain) -> return (subdomain, defaultDomain)
      Left err             -> throwM <| toServerError err

splashCID :: CID
splashCID = CID "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"

defaultDomain :: DomainName
defaultDomain = DomainName "fission.app"
