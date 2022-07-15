module Fission.Web.Server.App.Modifier.Class
  ( Modifier (..)
  , Errors'
  ) where

import           Database.Persist                                   as Persist
import           Servant.Server

import           Network.IPFS.Bytes.Types
import           Network.IPFS.CID.Types

import qualified Network.IPFS.Add.Error                             as IPFS.Pin
import qualified Network.IPFS.Files.Error                           as IPFS.Files
import qualified Network.IPFS.Get.Error                             as IPFS.Stat

import           Fission.Prelude                                    hiding (on)

import           Fission.Error                                      as Error
import           Fission.URL

import qualified Fission.Web.Server.IPFS.DNSLink.Class              as DNSLink
import           Fission.Web.Server.Error.ActionNotAuthorized.Types
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB.Types (Transaction)

import qualified Fission.Web.Auth.Token.UCAN.Resource.Types         as Ucan


type Errors' = OpenUnion
  '[ NotFound App
   , NotFound AppDomain
   , NotFound Domain
   , NotFound URL

   , ActionNotAuthorized App
   , ActionNotAuthorized AppDomain
   , ActionNotAuthorized URL
   , ActionNotAuthorized Ucan.Resource

   , IPFS.Files.Error
   , IPFS.Pin.Error
   , IPFS.Stat.Error

   , DNSLink.Errors'
   , ServerError
   , InvalidURL
   ]

class Monad m => Modifier m where
  setCIDDirectly ::
       UTCTime
    -> AppId
    -> Bytes
    -> CID
    -> m (Either Errors' AppId)

  setCID ::
       UserId  -- ^ User for auth
    -> URL     -- ^ URL associated with target app
    -> CID     -- ^ New CID
    -> Bool    -- ^ Flag: copy data (default yes)
    -> UTCTime -- ^ Now
    -> m (Either Errors' AppId)


instance MonadIO m => Modifier (Transaction m) where
  setCIDDirectly now appId size newCID = do
    update appId
      [ AppCid  =. newCID
      , AppSize =. size
      ]

    insert (SetAppCIDEvent appId newCID size now)
    return (Right appId)
