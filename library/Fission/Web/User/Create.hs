module Fission.Web.User.Create
  ( API
  , server
  ) where

import           Servant
import           Network.IPFS.CID.Types

import           Fission.Prelude
import           Fission.IPFS.DNSLink as DNSLink
import           Fission.Web.Error    as Web.Err

import qualified Fission.User as User

type API = ReqBody '[JSON] User.Registration
        :> Post    '[JSON] ()

server ::
  ( MonadDNSLink   m
  , MonadLogger    m
  , MonadTime      m
  , MonadDB      t m
  , User.Creator t
  )
  => ServerT API m
server (User.Registration username password email) = do
  Nothing
    |> User.create username password (Just email)
    |> runDBNow
    |> bind Web.Err.ensure
    |> void

  splashCID
    |> DNSLink.setWithSubdomain username
    |> bind Web.Err.ensureM
    |> void

splashCID :: CID
splashCID = CID "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"
