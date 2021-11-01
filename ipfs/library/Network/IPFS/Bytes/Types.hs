module Network.IPFS.Bytes.Types (Bytes(..)) where

import           Network.IPFS.Prelude

newtype Bytes = Bytes { unBytes :: Natural }
  deriving newtype ( Eq
                   , Show
                   )

instance FromJSON Bytes where
  parseJSON val = do
    nat <- parseJSON val
    return <| Bytes nat
