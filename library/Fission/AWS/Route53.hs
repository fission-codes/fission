module Fission.AWS.Route53 
  ( createChangeRequest
  -- , createAuth
  ) where

import RIO

import Network.AWS.Route53 as Route53
import Network.AWS.Prelude as AWS
import Network.AWS.Auth    as AWS.Auth
import Control.Monad.Trans.AWS
import           Fission.Internal.Constraint


import Control.Lens ((?~))

addValue :: ResourceRecordSet -> Text -> ResourceRecordSet
addValue recordSet value = do
  let record = resourceRecord value
  (recordSet & rrsResourceRecords ?~ toNonEmpty [record]) & rrsTTL ?~ (500 ::Natural)


createChangeRequest :: ChangeResourceRecordSets
createChangeRequest = do
  let recordSet = resourceRecordSet "test.runfission.com" Txt
  let updated = addValue recordSet "\"testing\""
  let changes = changeBatch $ toNonEmpty [change Upsert updated]
  let zone = ResourceId "Z23P2TYDHT4QDT"
  changeResourceRecordSets zone changes

-- createAuth :: MonadRIO cfg m
--           --  => MonadUnliftIO m
--            => m()
-- createAuth = do 
--   env <- newEnv $ FromKeys (AccessKey "access") (SecretKey "secret")
--   liftIO .runResourceT . runAWST env . within NorthVirginia $ do
--     res <- send(createChangeRequest)
--     undefined
