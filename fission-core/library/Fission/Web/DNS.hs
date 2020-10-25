module Fission.Web.DNS
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Servant

import           Network.IPFS.CID.Types

import           Fission.Prelude

import           Fission.Models

import qualified Fission.Authorization                       as Auth
import           Fission.Web.Auth.Token.UCAN.Privilege.Types

import           Fission.URL                                 as URL
import qualified Fission.Web.Error                           as Web.Err

import qualified Fission.User.Modifier                       as User
import           Fission.User.Username.Types

type API
  =  Summary "Set account's DNSLink"
  :> Description "DEPRECATED ⛔ Set account's DNSLink to a CID"
  :> Capture "cid" CID
  :> PutAccepted '[PlainText, OctetStream] DomainName

-- FIXME Do we want to make this work with the Domain resource?

-- Deprecated! Works the "old" way with direct access to username.fission.name,
-- WITHOUT the `files` prefix
server ::
  ( MonadTime     m
  , MonadThrow    m
  , MonadLogger   m
  , User.Modifier m
  )
  => Auth.Session
  -> ServerT API m
server = undefined -- FIXME
-- server Auth.Session {about = Entity userID User {userUsername = Username rawUN}} cid = do
--   now <- currentTime
--   Web.Err.ensureM $ User.setData userID cid now
--   return . DomainName $ rawUN <> ".fission.name"
