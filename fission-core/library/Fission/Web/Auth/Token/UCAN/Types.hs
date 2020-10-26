{-# LANGUAGE UndecidableInstances #-}

module Fission.Web.Auth.Token.UCAN.Types
  ( UCAN   (..)
  , Claims (..)
  , module Fission.Web.Auth.Token.JWT.Header.Types
  ) where

import qualified System.IO.Unsafe                             as Unsafe

import           Fission.Web.Auth.Token.UCAN.Attenuated.Types


import           Fission.Web.Auth.Token.JWT.RawContent.Class
import           Fission.Web.Auth.Token.UCAN.Proof.Types


import qualified Fission.Internal.Base64.URL                  as B64.URL



import           Fission.Web.Auth.Token.UCAN.Signature


import           Crypto.PubKey.Ed25519                        (toPublic)

import qualified Data.ByteString.Base64.URL                   as BS.B64.URL

import qualified RIO.ByteString.Lazy                          as Lazy
import qualified RIO.Text                                     as Text

import           Fission.Prelude

import qualified Fission.Key.Asymmetric.Algorithm.Types       as Algorithm

import qualified Fission.Internal.RSA2048.Pair.Types          as RSA2048
import qualified Fission.Internal.UTF8                        as UTF8

import           Fission.Key                                  as Key

import           Fission.User.DID.Types

import           Fission.Web.Auth.Token.JWT.Signature         as Signature

-- Reexports

import           Fission.Web.Auth.Token.JWT.Header.Types

import           Fission.Web.Auth.Token.JWT.RawContent.Types  as JWT

import           Fission.Web.Auth.Token.JWT.RawContent.Class

-- Orphans

import           Fission.Internal.Orphanage.CID               ()
import           Fission.Internal.Orphanage.Ed25519.SecretKey ()

data UCAN privilege fact = UCAN
  { header :: !Header
  , claims :: !(Claims privilege fact)
  , sig    :: !Signature.Signature
  } deriving (Show, Eq, Generic)

instance
  ( Arbitrary privilege
  , Arbitrary fact

  , ToJSON privilege
  , ToJSON fact

  -- , HasField' "signature" (UCAN privilege fact) Signature
  , ToRawContent          (UCAN privilege fact)
  )
  => Arbitrary (UCAN privilege fact) where
    arbitrary = do
      header   <- arbitrary
      (pk, sk) <- case alg header of
        Algorithm.RSA2048 -> do
          RSA2048.Pair pk' sk' <- arbitrary
          return (RSAPublicKey pk', Left sk')

        Algorithm.EdDSA -> do
          sk' <- arbitrary
          return (Ed25519PublicKey (toPublic sk'), Right sk')

      claims' <- arbitrary

      let
        claims = claims' {sender = DID Key pk}

        sig' = case sk of
          Left rsaSK -> Unsafe.unsafePerformIO $ signRS256 header claims rsaSK
          Right edSK -> Right $ signEdDSA header claims edSK

      case sig' of
        Left _    -> error "Unable to sign JWT"
        Right sig -> return UCAN {..}

instance
  ( ToJSON privilege
  , ToJSON fact
  )
  => ToRawContent (UCAN privilege fact) where
    toRawContent UCAN {..} = JWT.RawContent $ B64.URL.encodeJWT header claims

instance
  ( ToJSON privilege
  , ToJSON fact

  -- , HasField' "signature" (UCAN privilege fact) Signature
  )
  => ToJSON (UCAN privilege fact) where
  toJSON UCAN {..} = String $ content <> "." <> textDisplay sig
    where
      content = decodeUtf8Lenient $ encodeB64 header <> "." <> encodeB64 claims

      encodeB64 jsonable =
        jsonable
          |> encode
          |> Lazy.toStrict
          |> UTF8.stripQuotesBS
          |> BS.B64.URL.encode
          |> UTF8.stripPadding

instance
  ( FromJSON privilege
  , FromJSON fact
  )
  => FromJSON (UCAN privilege fact) where
    parseJSON = withText "JWT.Token" \txt ->
      case Text.split (== '.') txt of
        [rawHeader, rawClaims, rawSig] -> do
          header <- withEmbeddedJSON "Header" parseJSON $ jsonify rawHeader
          claims <- withEmbeddedJSON "Claims" parseJSON $ jsonify rawClaims
          sig    <- Signature.parse (alg header) (toJSON rawSig)
          return UCAN {..}

        _ ->
          fail $ "Wrong number of JWT segments in:  " <> Text.unpack txt
      where
        jsonify = toJSON . decodeUtf8Lenient . BS.B64.URL.decodeLenient . encodeUtf8

------------
-- Claims --
------------

data Claims privilege fact = Claims -- our use case: Claims [Privileege] Fact
  -- Dramatis Personae
  { sender       :: !DID
  , receiver     :: !DID
  -- Scope (set-like operations)
  , proofs       :: !(Proof (UCAN privilege fact)) -- ^ Input scope (self-certifying)
  , attenuations :: !(Attenuated [privilege]) -- ^ Output scope (certified by this UCAN)
  -- Additional signed info
  , facts        :: ![fact]
  -- Temporal Bounds
  , exp          :: !UTCTime
  , nbf          :: !UTCTime
  } deriving Show

instance
  ( Show privilege
  , Show fact
  )
  => Display (Claims privilege fact) where
    textDisplay = Text.pack . show

instance (Eq privilege, Eq fact) => Eq (Claims privilege fact) where
  ucanA == ucanB = eqWho && eqAuth && eqTime
    where
      eqWho = sender ucanA == sender   ucanB
         && receiver ucanA == receiver ucanB

      eqAuth = attenuations ucanA == attenuations ucanB
             &&      proofs ucanA == proofs       ucanB
             &&       facts ucanA == facts        ucanB

      eqTime = roundUTC (exp ucanA) == roundUTC (exp ucanB)
            && roundUTC (nbf ucanA) == roundUTC (nbf ucanB)

instance
  ( Arbitrary privilege
  , Arbitrary fact

  , ToJSON privilege
  , ToJSON fact

  -- , HasField' "signature" (UCAN privilege fact) Signature
  , ToRawContent          (UCAN privilege fact)
  )
  => Arbitrary (Claims privilege fact) where
    arbitrary = do
      sender     <- arbitrary
      receiverPK <- arbitrary

      exp <- arbitrary
      nbf <- arbitrary

      attenuations <- arbitrary
      proofs       <- arbitrary
      facts        <- arbitrary

      let
        receiver = DID
          { publicKey = receiverPK
          , method    = Key
          }

      return Claims {..}

instance
  ( ToJSON privilege
  , ToJSON fact

  -- , HasField' "signature" (UCAN privilege fact) Signature
  )
  => ToJSON (Claims privilege fact) where
    toJSON Claims {..} = object
      [ "iss" .= sender
      , "aud" .= receiver
      --
      , "prf" .= proofs
      , "att" .= attenuations
      , "fct" .= facts
      --
      , "nbf" .= toSeconds nbf
      , "exp" .= toSeconds exp
      ]

instance
  ( FromJSON privilege
  , FromJSON fact
  )
  => FromJSON (Claims privilege fact) where
    parseJSON = withObject "JWT.Payload" \obj -> do
      sender   <- obj .: "iss"
      receiver <- obj .: "aud"
      --
      attenuations <- obj .:  "att"
      proofs'      <- obj .:? "prf"
      facts        <- obj .:? "fct" .!= []
      --
      nbf <- fromSeconds <$> obj .: "nbf"
      exp <- fromSeconds <$> obj .: "exp"

      let
        proofs =
          case proofs' of
            Nothing   -> RootCredential
            Just []   -> RootCredential
            Just prfs -> DelegatedFrom prfs

      return Claims {..}
