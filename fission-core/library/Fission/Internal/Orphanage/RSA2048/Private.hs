{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.RSA2048.Private () where

import           Data.Binary         as Binary
import           Data.ByteArray      as ByteArray

import qualified RIO.ByteString.Lazy as Lazy

import qualified Codec.Crypto.RSA    as Codec.RSA
import qualified Crypto.PubKey.RSA   as RSA
import qualified System.IO.Unsafe    as Unsafe

import           Fission.Prelude

instance Arbitrary RSA.PrivateKey where
  arbitrary = do
    exp <- elements [3, 5, 17, 257, 65537]
    return . snd . Unsafe.unsafePerformIO $ RSA.generate 2048 exp

instance Binary RSA.PrivateKey where
  get = fromCodecRSA <$> get
  put = put . toCodecRSA

instance ByteArrayAccess RSA.PrivateKey where
  length        = ByteArray.length . Lazy.toStrict . Binary.encode . toCodecRSA
  withByteArray = withByteArray    . Lazy.toStrict . Binary.encode . toCodecRSA

toCodecRSA :: RSA.PrivateKey -> Codec.RSA.PrivateKey
toCodecRSA RSA.PrivateKey {private_pub = RSA.PublicKey {..}, ..} =
  Codec.RSA.PrivateKey
    { private_pub = Codec.RSA.PublicKey {..}
    , ..
    }

fromCodecRSA :: Codec.RSA.PrivateKey -> RSA.PrivateKey
fromCodecRSA Codec.RSA.PrivateKey {private_pub = Codec.RSA.PublicKey {..}, ..} =
  RSA.PrivateKey
    { private_pub = RSA.PublicKey {..}
    , ..
    }
