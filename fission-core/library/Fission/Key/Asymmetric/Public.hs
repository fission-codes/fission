module Fission.Key.Asymmetric.Public (genRSA2048) where

import qualified RIO.ByteString.Lazy                        as Lazy
import           RIO.FilePath                               ((</>))

import           Data.Binary                                as Binary
import           Data.ByteArray                             as ByteArray

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519                      as Ed25519
import qualified Crypto.PubKey.RSA                          as RSA
import           Crypto.Random.Types
import qualified OpenSSL.RSA                                as OpenSSL

import           Fission.Prelude

import           Fission.Key.Error                          as Key

import           Fission.Internal.Orphanage.RSA2048.Private ()

genRSA2048 :: MonadIO m => m RSA.PrivateKey
genRSA2048 = do
  pair <- liftIO $ OpenSSL.generateRSAKey' 2048 65537

  let
    public_size = OpenSSL.rsaSize pair
    public_n    = OpenSSL.rsaN pair
    public_e    = OpenSSL.rsaE pair

    private_pub = RSA.PublicKey {public_size, public_n, public_e}

    private_d = OpenSSL.rsaD pair
    private_p = OpenSSL.rsaP pair
    private_q = OpenSSL.rsaQ pair

  case OpenSSL.rsaDMP1 pair of
    Nothing ->
      genRSA2048

    Just private_dP ->
      case OpenSSL.rsaDMQ1 pair of
        Nothing ->
          genRSA2048

        Just private_dQ ->
          case OpenSSL.rsaIQMP pair of
            Nothing ->
              genRSA2048

            Just private_qinv ->
              return RSA.PrivateKey {..}
