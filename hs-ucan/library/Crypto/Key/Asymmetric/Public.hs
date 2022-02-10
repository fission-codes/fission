module Crypto.Key.Asymmetric.Public
  ( genRSA2048
  , decodeASN1DERRSAPublicKey
  , encodeASN1DERRSAPublicKey
  ) where

import qualified Crypto.PubKey.RSA                           as RSA

import qualified Data.ASN1.BitArray                          as ASN1
import qualified Data.ASN1.Types                             as ASN1
import qualified Data.ByteString                             as BS
import qualified Data.X509                                   as X509
import qualified OpenSSL.RSA                                 as OpenSSL
import           RIO
import           Web.UCAN.Internal.Orphanage.RSA2048.Private ()


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


decodeASN1DERRSAPublicKey :: ByteString -> Either String RSA.PublicKey
decodeASN1DERRSAPublicKey bs =
  let
    spki =
      [ ASN1.Start ASN1.Sequence
      , ASN1.Start ASN1.Sequence
      , ASN1.OID [1,2,840,113549,1,1,1]
      , ASN1.Null
      , ASN1.End ASN1.Sequence
      , ASN1.BitString (ASN1.toBitArray bs (BS.length bs * 8))
      , ASN1.End ASN1.Sequence
      ]
  in case ASN1.fromASN1 spki of
    Right (X509.PubKeyRSA pk, _) ->
      Right pk

    Right (pk, _) ->
      Left $ "Couldn't parse RSAPublicKey (ASN1 DER encoded). Different format provided: " <> show pk

    Left err ->
      Left $ show err


encodeASN1DERRSAPublicKey :: RSA.PublicKey -> ByteString
encodeASN1DERRSAPublicKey pk =
  case ASN1.toASN1 (X509.PubKeyRSA pk) [] of
    [ ASN1.Start ASN1.Sequence
      , ASN1.Start ASN1.Sequence
      , ASN1.OID [1,2,840,113549,1,1,1]
      , ASN1.Null
      , ASN1.End ASN1.Sequence
      , ASN1.BitString bitArray
      , ASN1.End ASN1.Sequence
      ] ->
        ASN1.bitArrayGetData bitArray

    _ ->
      error "Unexpected ASN1 SubjectPublicKeyInfo encoding of RSA public keys"
