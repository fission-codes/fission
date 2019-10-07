module Fission.AWS.Route53 
  ( createChangeRequest
  , registerDomain
  ) where

import RIO

import Data.Has
import qualified Fission.Config as Config

import Network.AWS.Route53 as Route53
import Network.AWS.Prelude as AWS
import Network.AWS.Auth    as AWS
import Control.Monad.Trans.AWS
import           Fission.Internal.Constraint
import           Fission.IPFS.CID.Types


import Control.Lens ((?~))

registerDomain :: MonadRIO cfg m
           => Has AWS.AccessKey    cfg
           => Has AWS.SecretKey    cfg
           => Text
           -> CID
           -> m(Bool)
registerDomain subdomain cid= do 
  env <- createEnv
  liftIO $ runResourceT . runAWST env . within NorthVirginia $ do
    let
      baseUrl = subdomain <> ".runfission.com"
      dnslinkUrl = "_dnslink." <> baseUrl
      dnslink = "ipfs/" <> (unaddress cid)

    _ <- send(createChangeRequest Cname baseUrl "ipfs.runfission.com")
    _ <- send(createChangeRequest Txt dnslinkUrl $ wrapQuotes dnslink)
    return True

createEnv :: MonadRIO cfg m
           => Has AWS.AccessKey    cfg
           => Has AWS.SecretKey    cfg
           => m(Env)
createEnv = do
  AccessKey accessKey <- Config.get
  SecretKey secretKey <- Config.get
  liftIO $ newEnv $ FromKeys (AccessKey accessKey) (SecretKey secretKey)

createChangeRequest :: RecordType
                    -> Text
                    -> Text
                    -> ChangeResourceRecordSets
createChangeRequest recordType domain content = do
  let recordSet = resourceRecordSet domain recordType
  let updated = addValue recordSet content
  let changes = changeBatch $ toNonEmpty [change Upsert updated]
  let zone = ResourceId "Z23P2TYDHT4QDT"
  changeResourceRecordSets zone changes

addValue :: ResourceRecordSet -> Text -> ResourceRecordSet
addValue recordSet value = do
  let record = resourceRecord value
  (recordSet & rrsResourceRecords ?~ toNonEmpty [record]) & rrsTTL ?~ (300 ::Natural)

wrapQuotes :: Text -> Text
wrapQuotes txt = "\"" <> txt <> "\""
