module Fission.Web.User.Create
  ( API
  , server
  ) where

import           Servant

import           Network.IPFS.CID.Types

import           Fission.Prelude
import           Fission.Web.Error as Web.Err

import qualified Fission.User.Mutation            as User
import qualified Fission.User.Registration.Types  as User

import qualified Fission.AWS.Types   as AWS
import           Fission.IPFS.DNSLink.Class as DNSLink

type API = ReqBody '[JSON] User.Registration
        :> Post '[JSON] ()

server ::
  ( MonadDNSLink  m
  , MonadDB       m
  , MonadLogger   m
  , MonadTime     m
  )
  => ServerT API m
server (User.Registration username password email) = do
  createResponse <- runDBNow \now ->
    User.create username password (Just email) Nothing now

  void <| Web.Err.ensure <| createResponse
  void <| Web.Err.ensureM =<< DNSLink.set (Just (AWS.Subdomain username)) splashCID

splashCID :: CID
splashCID = CID "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"
