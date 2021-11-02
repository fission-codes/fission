module Network.IPFS
  ( MonadLocalIPFS
  , runLocal
  , MonadRemoteIPFS
  , runRemote
  , ipfsAdd
  , ipfsCat
  , ipfsPin
  , ipfsUnpin
  ) where

import Network.IPFS.Local.Class
import Network.IPFS.Remote.Class
