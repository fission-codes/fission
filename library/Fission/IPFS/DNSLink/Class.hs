module Fission.IPFS.DNSLink.Class
  ( MonadDNSLink (..)
  , Errors'
  ) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude           hiding (set)

import           Fission.Error.Types
import           Fission.Models
import           Fission.URL

import           Fission.AWS.Route53.Class
import           Fission.AWS.Types         as AWS

type Errors' = OpenUnion
  '[ ServerError
   , InvalidURL

   , NotFound            URL
   , ActionNotAuthorized URL
   ]

-- | Low-level 'DNSLink' interface
class MonadRoute53 m => MonadDNSLink m where
  set ::
       UserId     -- ^ Who is performing this action (for auth)
    -> URL        -- ^ The @URL@ target
    -> AWS.ZoneID -- ^ The @ZoneID@ for the associated @URL@
    -> CID        -- ^ The @CID@ to set at that DNSLink
    -> m (Either Errors' URL)

  follow ::
       UserId
    -> URL        -- ^ Follower
    -> AWS.ZoneID -- ^ Follower Zone
    -> Path URL   -- ^ Followee
    -> m (Either Errors' ())
