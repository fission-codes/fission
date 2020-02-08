module Fission.Web.User.Create
  ( API
  , server
  ) where

import           Servant

import           Fission.Prelude
import           Fission.Web.Error as Web.Err

import           Fission.IPFS.DNSLink   as DNSLink
import           Network.IPFS.CID.Types

import qualified Fission.User as User
import           Fission.User.DID.Types
import           Fission.User.Username.Types

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
server did (User.Registration username@(Username rawUN) email) = do
  Nothing
    |> User.create username did (Just email)
    |> runDBNow
    |> bind Web.Err.ensure
    |> void

  splashCID
    |> DNSLink.setWithSubdomain rawUN
    |> bind Web.Err.ensureM
    |> void

splashCID :: CID
splashCID = CID "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"
