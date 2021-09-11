module Fission.Web.API.IPFS.Types (RoutesV_ (..), RoutesV2 (..)) where

import qualified Network.IPFS.CID.Types              as IPFS
import qualified Network.IPFS.File.Types             as File

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types          as Auth

import qualified Fission.Web.API.IPFS.CID.Types      as CID
import qualified Fission.Web.API.IPFS.DAG.Types      as DAG
import qualified Fission.Web.API.IPFS.Download.Types as Download
import qualified Fission.Web.API.IPFS.Peer.Types     as Peer
import qualified Fission.Web.API.IPFS.Pin.Types      as Pin

newtype RoutesV2 mode = RoutesV2 { peers :: mode :- "peers" :> ToServantApi Peer.Routes }
  deriving Generic

data RoutesV_ mode = RoutesV_
  { cid      :: mode :- "cid"   :> ToServantApi CID.Routes
  , dag      :: mode :- "dag"   :> ToServantApi DAG.Routes
  , peers    :: mode :- "peers" :> ToServantApi Peer.Routes
  , pin      :: mode :-            ToServantApi Pin.Routes
  , download :: mode :-            ToServantApi Download.Routes

  , upload ::
      mode
      :- Summary "Upload file"
      :> Description "Directly upload a file over HTTP"
      --
      :> ReqBody '[PlainText, OctetStream] File.Serialized
      --
      :> Auth.HigherOrder
      :> Post '[PlainText, OctetStream] IPFS.CID
  }
  deriving Generic
