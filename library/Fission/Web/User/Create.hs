module Fission.Web.User.Create
  ( API
  , server
  ) where

import           Fission.Prelude

import           Servant

import           Fission.IPFS.DNSLink as DNSLink
import qualified Fission.User as User
import           Network.IPFS.CID.Types

import           Fission.Web.Error as Web.Err

import           Fission.User.DID.Types

type API = ReqBody '[JSON] User.Registration
        :> Post    '[JSON] ()

server ::
  ( MonadDNSLink   m
  , MonadLogger    m
  , MonadTime      m
  , MonadDB      t m
  , User.Creator t
  )
  => DID
  -> ServerT API m
server did (User.Registration username email) = do
  Nothing
    |> User.create username (Left did) (Just email)
    |> runDBNow
    |> bind Web.Err.ensure
    |> void

  splashCID
    |> DNSLink.setWithSubdomain username
    |> bind Web.Err.ensureM
    |> void

splashCID :: CID
splashCID = CID "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"
