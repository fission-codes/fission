module Fission.IPFS.Peer.Types (Peer (..)) where

import RIO

import Control.Lens ((.~), (?~))
import Data.Aeson
import Data.Aeson.TH
import Data.Swagger ( NamedSchema (..)
                    , SwaggerType (..)
                    , ToSchema (..)
                    , type_
                    , example
                    )

newtype Peer = Peer { peer :: Text }
  deriving          ( Show
                    , Generic
                    )
  deriving newtype  ( IsString )

$(deriveJSON defaultOptions ''Peer)

instance ToSchema Peer where
  declareNamedSchema _ =
     return $ NamedSchema (Just "IPFS Peer") $ mempty
            & type_   ?~ SwaggerString
            & example ?~ "/ip4/178.62.158.247/tcp/4001/ipfs/QmSoLer265NRgSp2LA3dPaeykiS1J6DifTC88f5uVQKNAd"
