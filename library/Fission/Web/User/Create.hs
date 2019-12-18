module Fission.Web.User.Create
  ( API
  , server
  ) where

import           Servant

import           Network.AWS.Auth    as AWS
import qualified Network.IPFS.Types as IPFS
import           Network.IPFS.CID.Types

import           Fission.Prelude
import           Fission.Web.Error as Web.Err

import qualified Fission.User.Mutation            as User
import qualified Fission.User.Registration.Types  as User

import qualified Fission.AWS.Types   as AWS
import           Fission.AWS.Route53

type API = ReqBody '[JSON] User.Registration
        :> Post '[JSON] ()

server
  :: ( Has IPFS.Gateway           cfg
     , Has AWS.DomainName         cfg
     , Has AWS.AccessKey          cfg
     , Has AWS.SecretKey          cfg
     , Has AWS.ZoneID             cfg
     , Has AWS.Route53MockEnabled cfg
     , MonadReader                cfg m
     , MonadDB       m
     , MonadLogger   m
     , MonadUnliftIO m
     , MonadThrow    m
     , MonadTime     m
     )
  => ServerT API m
server (User.Registration username password email) = do
  createResponse <- runDBNow <| User.create username password email Nothing
  case createResponse of
    Left err -> Web.Err.throw err
    Right _userId ->
      void . Web.Err.ensureM =<< registerDomain username splashCID
  -- void . Web.Err.ensureM =<< (runDBNow <| User.create username password email Nothing)
  -- void . Web.Err.ensureM =<< registerDomain username splashCID

splashCID :: CID
splashCID = CID "QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN"
