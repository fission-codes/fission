module Web.UCAN.Claims.Types
  ( Claims(..)
  , arbitraryClaims
  ) where

import           Data.Aeson
import           RIO                                           hiding (exp)
import qualified RIO.Text                                      as Text
import           RIO.Time
import           Test.QuickCheck
import           Web.DID.Types                                 as DID
import           Web.UCAN.Capabilities.Class
import           Web.UCAN.Capabilities.Types
import           Web.UCAN.Internal.Orphanage.Ed25519.SecretKey ()
import           Web.UCAN.Internal.Time
import           Web.UCAN.Nonce.Types
import           Web.UCAN.Proof.Types


data Claims fct res abl = Claims
  -- Dramatis Personae
  { sender      :: DID
  , receiver    :: DID
  -- Authorization Target
  , attenuation :: [Capability res abl]
  , proofs      :: [Proof]
  , facts       :: [fct]
  -- Temporal Bounds
  , expiration  :: UTCTime
  , notBefore   :: Maybe UTCTime
  , nonce       :: Maybe Nonce
  } deriving (Show)

instance (Show fct, Show res, Show abl) => Display (Claims fct res abl) where
  textDisplay = Text.pack . show

instance (Eq fct, Eq res, Eq abl) => Eq (Claims fct res abl) where
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
  ) => Gen [Proof] -> DID -> Gen (Claims fct res abl)
arbitraryClaims arbitraryProofs sender = do
  receiver    <- DID.Key <$> arbitrary
  attenuation <- arbitrary
  proofs      <- arbitraryProofs
  facts       <- arbitrary
  expiration  <- arbitrary
  notBefore   <- arbitrary
  nonce       <- arbitrary

  return Claims {..}

