module Fission.AWS.Response.Class (Response (..)) where

import RIO
import Network.AWS.Route53
import Network.AWS.CertificateManager

class Response a where
  status :: a -> Int

instance Response ChangeResourceRecordSetsResponse where
  status res = res ^. crrsrsResponseStatus

instance Response CreateHostedZoneResponse where
  status res = res ^. chzrsResponseStatus

instance Response RequestCertificateResponse where
  status res = res ^. rcrsResponseStatus

instance Response DescribeCertificateResponse where
  status res = res ^. dcrsResponseStatus
