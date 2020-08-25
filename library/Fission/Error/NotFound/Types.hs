module Fission.Error.NotFound.Types (NotFound (..)) where

import qualified Crypto.PubKey.Ed25519   as Ed25519
import qualified Network.IPFS.Types      as IPFS
import           Servant

import qualified Fission.AWS.Zone.Types  as AWS
import           Fission.Models
import           Fission.Prelude
import           Fission.URL
import           Fission.Web.Error.Class

data NotFound entity
  = NotFound
  deriving ( Show
           , Eq
           , Exception
           )

instance ToServerError (NotFound entity) where
  toServerError _ = err404

instance Display (NotFound User) where
  display _ = "User not found"

instance Display (NotFound UserChallenge) where
  display _ = "Challenge not found"

instance Display (NotFound LoosePin) where
  display _ = "Loose pin not found"

instance Display (NotFound Domain) where
  display _ = "Domain not found in system"

instance Display (NotFound URL) where
  display _ = "URL not found in system"

instance Display (NotFound App) where
  display _ = "App not found"

instance Display (NotFound AppDomain) where
  display _ = "App/Domain relation not found"

instance Display (NotFound AWS.ZoneID) where
  display _ = "AWS.ZoneID not found"

instance Display (NotFound [IPFS.Peer]) where
  display _ = "Unable to find IPFS peers"

instance Display (NotFound Ed25519.SecretKey) where
  display _ = "Unable to find Ed25519 secret key"
