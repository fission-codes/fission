module Fission.Web.App
  ( API
  , server
  ) where

-- import           Network.IPFS.CID.Types
import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models

import           Fission.IPFS.DNSLink.Class as DNSLink
-- import qualified Fission.URL.Types          as URL
-- import           Fission.Web.Error          as Web.Err
-- import           Fission.User.Username.Types

import qualified Fission.App.Creator.Class  as App

import qualified Fission.Web.App.Create  as Create
-- import qualified Fission.Web.App.Destroy as Destroy

type API
  =    Create.API
  -- :<|> Destroy.API

server ::
  ( MonadTime      m
  , MonadDNSLink   m
  , MonadDB      t m
  , App.Creator  t
  )
  => Entity User
  -> ServerT API m
server = Create.create
   -- :<|> Destroy.destroy

-- type DomainCreateAPI
--   =  Capture "domain" DomainName
--   :> PutAccepted '[PlainText, OctetStream, JSON] NoResponse

-- type PurchaseDomainAPI
--   =  Capture "domain" DomainName
--   :> "purchase"
--   :> PutCreated '[PlainText, OctetStream, JSON] NoResponse

-- type UnregisterDomainAPI
--   =  Capture "domain" DomainName
--   :> Delete '[PlainText, OctetStream, JSON] NoResponse

-- type TransferDomainAPI
--   =  Capture "domain" DomainName
--   :> "purchase"
--   :> PutCreated '[PlainText, OctetStream, JSON] NoResponse

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
