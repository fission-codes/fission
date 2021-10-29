module Network.IPFS.DAG.Link (create) where

import           Network.IPFS.Prelude
import           Network.IPFS.Local.Class

import           Network.IPFS.Get.Error      as IPFS.Get
import           Network.IPFS.Types          as IPFS
import           Network.IPFS.DAG.Link.Types as DAG
import qualified Network.IPFS.Stat           as Stat

create ::
  MonadLocalIPFS m
  => IPFS.CID
  -> IPFS.Name
  -> m (Either IPFS.Get.Error Link)
create cid name = Stat.getSize cid >>= \case
  Left err -> return <| Left err
  Right size -> return . Right <| Link
    { cid = cid
    , name = name
    , size = size
    }
