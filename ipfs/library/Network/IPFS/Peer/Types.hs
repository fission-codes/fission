module Network.IPFS.Peer.Types (Peer (..)) where

import           RIO

import           Control.Lens
import           Data.Aeson
import           Data.Swagger
import           Servant.API

import qualified Network.IPFS.Internal.UTF8 as UTF8

newtype Peer = Peer { peer :: Text }
  deriving          ( Eq
                    , Show
                    )
  deriving newtype  ( Display
                    , IsString
                    , FromJSON
                    )

instance ToJSON Peer where
  toJSON = String . peer

instance ToSchema Peer where
  declareNamedSchema _ =
     return $ NamedSchema (Just "IPFSPeer") $ mempty
            & type_       ?~ SwaggerString
            & example     ?~ "/ip4/178.62.158.247/tcp/4001/ipfs/QmSoLer265NRgSp2LA3dPaeykiS1J6DifTC88f5uVQKNAd"
            & description ?~ "An IPFS peer address"

instance MimeRender PlainText Peer where
  mimeRender _ = UTF8.textToLazyBS . peer

instance MimeRender OctetStream Peer where
  mimeRender _ = UTF8.textToLazyBS . peer
