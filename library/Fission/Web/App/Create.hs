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

import           Fission.IPFS.DNSLink.Class as DNSLink
import qualified Fission.URL.Types          as URL
import           Fission.Web.Error          as Web.Err
import           Fission.User.Username.Types

type API
  =  QueryParam "subdomain" Subdomain
  :> QueryParam "domain"    DomainName
  :> QueryParam "cid"       CID
  :> PostAccepted '[JSON] AppId

create :: MonadDNSLink m => Entity User -> ServerT API m
create (Entity userId _) mayRawSubdomain mayDomain mayCID = do
  (maySubdomain, domain) <- mkDomains
  -- FIXME ensure rights for domain

  -- -- create AppDomain with subdomain (it's a maybe, so just include directly)

  insertUnique
    |>

  -- -- set DNS
  return undefined
  where
    cid :: CID
    cid = maybe splashCID identity mayCID

    mkDomains :: MonadIO m => m (Maybe Subdomain, DomainName)
    mkDomains = case (mayRawSubdomain, mayDomain) of
      (Just subdomain, Just customDomain) -> return (Just subdomain, customDomain)
      (Nothing,        Just customDomain) -> return (Nothing,        customDomain)
      (Just subdomain, Nothing)           -> return (Just subdomain, defaultDomain)
      (Nothing,        Nothing) -> do
        subdomain <- liftIO <| generate arbitrary
        return (Just subdomain, defaultDomain)

splashCID :: CID
splashCID = CID "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"

defaultDomain :: DomainName
defaultDomain = DomainName "fission.app"
