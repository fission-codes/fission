{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.User.Create
  ( API
  , server
  ) where

import           Database.Selda as Selda
import           Servant

import           Fission.Prelude

import           Fission.Web.Server
import           Fission.Web.Error as Web.Err

import qualified Fission.User                     as User
import qualified Fission.User.Registration.Types  as User

import qualified Fission.AWS.Types   as AWS
import           Fission.AWS.Route53

import           Network.AWS.Auth    as AWS

import qualified Fission.IPFS.Types as IPFS
import           Fission.IPFS.CID.Types

type API = ReqBody '[JSON] User.Registration
        :> Post '[JSON] ()

server
  :: ( HasLogFunc         cfg
     , Has IPFS.Gateway   cfg
     , Has AWS.DomainName cfg
     , Has AWS.AccessKey  cfg
     , Has AWS.SecretKey  cfg
     , Has AWS.ZoneID     cfg
     , MonadSelda    (RIO cfg)
     )
  => RIOServer cfg API
server (User.Registration username password email) = do
  userID <- User.create username password email
  logInfo ("Provisioned user: " <> displayShow userID)
  registerDomain username splashCID >>= Web.Err.ensureM
  return ()

splashCID :: CID
splashCID = CID "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"
