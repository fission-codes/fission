module Fission.IPFS.DAG.Link (create) where

import           Fission.Prelude

import           Fission.IPFS.Error          as IPFS.Error
import           Fission.IPFS.Types          as IPFS
import           Fission.IPFS.DAG.Link.Types as DAG
import qualified Fission.IPFS.Stat           as Stat

create ::
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => IPFS.CID
  -> IPFS.Name
  -> m (Either IPFS.Error.Add Link)
create cid name = Stat.getSize cid >>= \case
  Left err -> return <| Left err
  Right size -> return . Right <| Link
    { cid = cid
    , name = name
    , size = size
    }
