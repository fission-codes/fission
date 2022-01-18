module Crypto.Key.Asymmetric.Public (genRSA2048) where

import qualified Crypto.PubKey.RSA                       as RSA
import qualified OpenSSL.RSA                             as OpenSSL
import           RIO
import           Ucan.Internal.Orphanage.RSA2048.Private ()

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
