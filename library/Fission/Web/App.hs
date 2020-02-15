module Fission.Web.App
  ( API
  , server
  ) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models

import           Fission.IPFS.DNSLink.Class as DNSLink
import qualified Fission.URL.Types          as URL
import           Fission.Web.Error          as Web.Err
import           Fission.User.Username.Types

type CreateAPI = QueryParam "subdomain" Subdomain
              :> QueryParam "domain"    DomainName
              :> QueryParam "cid"       CID
              :> PutAccepted '[PlainText, OctetStream] NoResponse

create :: MonadDNSLink m => Entity User -> ServerT API m
create (Entity userId _) mayRawSubdomain mayDomain mayCID = do
  (maySubdomain, domain) <- mkDomains
  -- ensure rights for domain
  -- create AppDomain with subdomain (it's a maybe, so just include directly)
  -- set DNS
  return NoContent
  where
    cid :: CID
    cid = maybe defaultCID mayCID

    defaultCID :: CID
    defaultCID = "Qm12345"

    mkDomains :: m (Maybe Subdomain, DomainName)
    mkDomains = case (mayRawSubdomain, mayDomain) of
      (Just subdomain, Just customDomain) -> return (Just subdomain, customDomain)
      (Nothing,        Just customDomain) -> return (Nothing,        customDomain)
      (Just subdomain, Nothing) -> return (subdomain, fission.name)
      (Nothing,        Nothing) -> do
        subdomain <- mkRandomSubdomain
        return (Just subdomain, fission.name)

    opinion    = []
    size       = []
    age        = []
    shape      = []
    colour     = []
    origin     = []
    material   = []
    purpose    = []

    nouns      = []

{-
  DOMAIN NAME
    REGSITER
      PUT /domain/register/foo.com

    PURCHASE
      PUT /domain/purchase/foo.com

  APP
    CREATE
      PUT /app -- autogenerate app name & cid

      PUT /app?domain=fission.name&subdoman=pretzelfacts&cid=Qm12345

      PUT /app/boris.fission.name
      PUT /app/boris.fission.name/Qmabcdef

-}
