module Fission.IPFS.Peer.Types (Peer (..)) where

import RIO

import Control.Lens ((?~))
import Data.Aeson
import Data.Aeson.TH
import Servant
import Data.Swagger ( NamedSchema (..)
                    , SwaggerType (..)
                    , ToSchema (..)
                    , type_
                    , example
                    )


import qualified Fission.Internal.UTF8 as UTF8

newtype Peer = Peer { peer :: Text }
  deriving          ( Eq
                    , Show
                    , Generic
                    )
  deriving newtype  ( Display
                    , IsString
                    , FromJSON
                    )

-- $(deriveJSON defaultOptions ''Peer)
instance ToJSON Peer where
  toJSON = String . peer

instance ToSchema Peer where
  declareNamedSchema _ =
     return $ NamedSchema (Just "IPFS Peer") $ mempty
            & type_   ?~ SwaggerString
            & example ?~ "/ip4/178.62.158.247/tcp/4001/ipfs/QmSoLer265NRgSp2LA3dPaeykiS1J6DifTC88f5uVQKNAd"

instance MimeRender PlainText Peer where
  mimeRender _ = UTF8.textToLazyBS . peer

instance MimeRender OctetStream Peer where
  mimeRender _ = UTF8.textToLazyBS . peer