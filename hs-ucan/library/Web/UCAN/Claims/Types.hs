module Web.UCAN.Claims.Types
  ( Claims(..)
  , arbitraryClaims
  ) where

import qualified System.IO.Unsafe                              as Unsafe

import           Crypto.Hash.Algorithms                        (SHA256 (..))
import           Crypto.Random                                 (MonadRandom (..))

import qualified Crypto.PubKey.RSA                             as RSA
import qualified Crypto.PubKey.RSA.PKCS15                      as RSA.PKCS15

import           Crypto.PubKey.Ed25519                         (toPublic)
import qualified Crypto.PubKey.Ed25519                         as Ed25519

import           Data.Aeson
import qualified Data.Aeson.Types                              as JSON
import qualified Data.ByteString.Base64.URL                    as BS.B64.URL
import qualified Data.Text.Encoding.Base64.URL                 as Text.B64.URL


import           RIO                                           hiding (exp)
import qualified RIO.ByteString.Lazy                           as Lazy
import qualified RIO.Text                                      as Text
import           RIO.Time

import qualified Servant.API                                   as Servant


import           Test.QuickCheck


import           Crypto.Key.Asymmetric                         as Key
import qualified Crypto.Key.Asymmetric.Algorithm.Types         as Algorithm

import           Web.DID.Types                                 as DID

import           Web.SemVer.Types
import qualified Web.UCAN.Header.Typ.Types                     as Typ

import           Web.UCAN.Signature                            as Signature
import qualified Web.UCAN.Signature.RS256.Types                as RS256

import           Web.UCAN.Capabilities.Class
import qualified Web.UCAN.Internal.Base64.URL                  as B64.URL
import           Web.UCAN.Internal.Orphanage.Ed25519.SecretKey ()
import qualified Web.UCAN.Internal.RSA2048.Pair.Types          as RSA2048
import           Web.UCAN.Internal.Time
import qualified Web.UCAN.Internal.UTF8                        as UTF8

import           Web.UCAN.Capabilities.Types
import           Web.UCAN.Header.Types
import           Web.UCAN.Nonce.Types
import           Web.UCAN.RawContent
import           Web.UCAN.Witness.Types


data Claims fct cap abl = Claims
  -- Dramatis Personae
  { sender      :: DID
  , receiver    :: DID
  -- Authorization Target
  , attenuation :: [Capability cap abl]
  , proofs      :: [Witness]
  , facts       :: [fct]
  -- Temporal Bounds
  , expiration  :: UTCTime
  , notBefore   :: Maybe UTCTime
  , nonce       :: Maybe Nonce
  } deriving (Show)

instance (Show fct, Show cap, Show abl) => Display (Claims fct cap abl) where
  textDisplay = Text.pack . show

instance (Eq fct, Eq cap, Eq abl) => Eq (Claims fct cap abl) where
  jwtA == jwtB = eqWho && eqAuth && eqTime && eqFacts && eqNonce
    where
      eqWho = sender jwtA == sender   jwtB
         && receiver jwtA == receiver jwtB

      eqAuth = attenuation jwtA == attenuation jwtB
            &&      proofs jwtA == proofs      jwtB

      eqTime = roundUTC (expiration jwtA) ==      roundUTC (expiration jwtB)
       && fmap roundUTC (notBefore  jwtA) == fmap roundUTC (notBefore  jwtB)

      eqFacts = facts jwtA == facts jwtB

      eqNonce = nonce jwtA == nonce jwtB


instance (ToJSON fct, IsResource res, IsAbility abl) => ToJSON (Claims fct res abl) where
  toJSON Claims {..} = object
    [ "iss" .= sender
    , "aud" .= receiver
    --
    , "prf" .= proofs
    , "att" .= attenuation
    , "fct" .= facts
    --
    , "exp" .=      toSeconds expiration
    , "nbf" .= fmap toSeconds notBefore
    , "nnc" .= nonce
    ]

instance
  ( FromJSON fct
  , IsResource res
  , IsAbility abl
  ) => FromJSON (Claims fct res abl) where
  parseJSON = withObject "JWT.Payload" \obj -> do
    sender   <- obj .: "iss"
    receiver <- obj .: "aud"
    --
    attenuation <- obj .:? "att" .!= []
    proofs      <- obj .:  "prf"
    facts       <- obj .:? "fct" .!= []
    --
    expiration <-      fromSeconds <$> obj .:  "exp"
    notBefore  <- fmap fromSeconds <$> obj .:? "nbf" .!= Nothing
    --
    nonce <- obj .:? "nnc" .!= Nothing

    return Claims {..}



arbitraryClaims ::
  ( Arbitrary fct
  , Arbitrary res
  , Arbitrary abl
  , ToJSON fct
  , IsResource res
  , IsAbility abl
  ) => Gen [Witness] -> DID -> Gen (Claims fct res abl)
arbitraryClaims arbitraryWitnesses sender = do
  receiver    <- DID.Key <$> arbitrary
  attenuation <- arbitrary
  proofs      <- arbitraryWitnesses
  facts       <- arbitrary
  expiration  <- arbitrary
  notBefore   <- arbitrary
  nonce       <- arbitrary

  return Claims {..}

