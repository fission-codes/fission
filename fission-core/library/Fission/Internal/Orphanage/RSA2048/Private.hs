{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.RSA2048.Private () where

import qualified RIO.ByteString.Lazy as Lazy

import           Data.Binary         as Binary
import           Data.ByteArray      as ByteArray

import qualified Codec.Crypto.RSA    as Codec.RSA
import qualified Crypto.PubKey.RSA   as RSA
import qualified OpenSSL.RSA         as OpenSSL

import qualified System.IO.Unsafe    as Unsafe

import           Fission.Prelude

instance Arbitrary RSA.PrivateKey where
  arbitrary =
    let
      pair = Unsafe.unsafePerformIO $ OpenSSL.generateRSAKey' 2048 65537

      public_size = OpenSSL.rsaSize pair
      public_n    = OpenSSL.rsaN pair
      public_e    = OpenSSL.rsaE pair

      private_pub = RSA.PublicKey {public_size, public_n, public_e}

      private_d = OpenSSL.rsaD pair
      private_p = OpenSSL.rsaP pair
      private_q = OpenSSL.rsaQ pair

   in
      case OpenSSL.rsaDMP1 pair of
        Nothing ->
          arbitrary

        Just private_dP ->
          case OpenSSL.rsaDMQ1 pair of
            Nothing ->
              arbitrary

            Just private_dQ ->
              case OpenSSL.rsaIQMP pair of
                Nothing ->
                  arbitrary

                Just private_qinv ->
                  return RSA.PrivateKey {..}

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
