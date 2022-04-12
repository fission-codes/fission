module Web.UCAN.Capabilities.Types
  ( Capability (..)
  , OwnedResources (..)
  , OwnershipScope (..)
  , Ability (..)
  , ProofRedelegation (..)
  ) where

import           Data.Aeson
import qualified Data.Aeson.Types                              as JSON
import qualified Data.Attoparsec.Text                          as Parse
import           RIO                                           hiding (exp)
import qualified RIO.Char                                      as Char
import qualified RIO.Text                                      as Text
import           Test.QuickCheck
import           Test.QuickCheck.Instances                     ()
import qualified Text.URI                                      as URI
import           Web.DID.Types                                 as DID
import           Web.UCAN.Capabilities.Class
import           Web.UCAN.Internal.Orphanage.Ed25519.SecretKey ()
import           Web.UCAN.Proof.Class



-- | A representation of capabilities in UCANs.
data Capability resource ability
  = CapResource resource (Ability ability)
  | CapOwnedResources (OwnedResources ability)
  | CapProofRedelegation ProofRedelegation
  deriving (Show, Eq, Ord)

-- | The "wnfs/APPEND" part of a capability as in { with: "wnfs://...", can: "wnfs/APPEND" }
data Ability ability
  = SuperUser       -- ^ represents can: "*"
  | Ability ability -- ^ represents any other can: "scope/action" pair
  deriving (Show, Eq, Ord)

-- | Represents resources of the form "my:<ability>" or "as:<did>:<ability>" and their associated ability
data OwnedResources ability
  = OwnedResources (Maybe DID) (OwnershipScope ability)
  deriving (Show, Eq, Ord)

-- | Represents the set of abilities referred to in "my:<ability>"
-- | or "as:<did>:<ability>" capabilities.
data OwnershipScope ability
  = All
    -- ^ the "*" in "my:*". The whole capability *must* be "{ with: "my:*", can: "*" }" or the equivalent with "as:..."
  | OnlyScheme (URI.RText 'URI.Scheme) (Ability ability)
    -- ^ e.g. the "wnfs" in "my:wnfs" or "as:<did>:wnfs"
  deriving (Show, Eq, Ord)

-- | Represents the "with" in e.g. { with: "prf:*", can: "ucan/DELEGATE" }
data ProofRedelegation
  = RedelegateAllProofs     -- ^ prf:*
  | RedelegateProof Natural -- ^ e.g. prf:0 or prf:10
  deriving (Show, Eq, Ord)


instance DelegationSemantics ability => DelegationSemantics (OwnershipScope ability) where
  All `canDelegate` _   = True
  _   `canDelegate` All = False
  (OnlyScheme aScheme aAbility) `canDelegate` (OnlyScheme bScheme bAbility) =
    aScheme == bScheme && aAbility `canDelegate` bAbility

instance DelegationSemantics ability => DelegationSemantics (Ability ability) where
  SuperUser   `canDelegate` _           = True
  _           `canDelegate` SuperUser   = False
  (Ability a) `canDelegate` (Ability b) = a `canDelegate` b

instance (Arbitrary res, Arbitrary abl) => Arbitrary (Capability res abl) where
  arbitrary =
    oneof
      [ CapResource <$> arbitrary <*> arbitrary
      , CapOwnedResources <$> arbitrary
      , CapProofRedelegation <$> arbitrary
      ]

instance Arbitrary ProofRedelegation where
  arbitrary =
    oneof
      [ pure RedelegateAllProofs
      , RedelegateProof <$> arbitrary
      ]

instance Arbitrary ability => Arbitrary (OwnedResources ability) where
  arbitrary = OwnedResources <$> arbitrary <*> arbitrary

instance Arbitrary ability => Arbitrary (OwnershipScope ability) where
  arbitrary =
    oneof
      [ pure All
      , OnlyScheme <$> arbitrary <*> arbitrary
      ]

instance ToJSON ProofRedelegation where
  toJSON RedelegateAllProofs = object
    [ "with" .= String "prf:*"
    , "can"  .= String "ucan/DELEGATE"
    ]
  toJSON (RedelegateProof n) = object
    [ "with" .= String ("prf:" <> Text.pack (show n))
    , "can"  .= String "ucan/DELEGATE"
    ]

instance Arbitrary ability => Arbitrary (Ability ability) where
  arbitrary =
    frequency
      [ (5, return SuperUser)
      , (1, Ability <$> arbitrary)
      ]

instance IsAbility ability => Display (Ability ability) where
  textDisplay SuperUser         = "*"
  textDisplay (Ability ability) = renderAbility ability

instance IsAbility abl => FromJSON (Ability abl) where
  parseJSON value = value & withText "UCAN.Ability" \can ->
    if can == "*"
      then return SuperUser
      else Ability <$> case parseAbility can of
        JSON.Success ability -> return ability
        JSON.Error message   -> fail message

instance IsAbility ability => ToJSON (OwnedResources ability) where
  toJSON (OwnedResources maybeDID All) = object
    [ "with" .= String (myOrAsPrefix maybeDID <> "*")
    , "can"  .= String "*"
    ]
  toJSON (OwnedResources maybeDID (OnlyScheme scheme ability)) = object
    [ "with" .= String (myOrAsPrefix maybeDID <> URI.unRText scheme)
    , "can"  .= String (textDisplay ability)
    ]

myOrAsPrefix :: Maybe DID -> Text
myOrAsPrefix = \case
  Just did -> "as:" <> textDisplay did <> ":"
  Nothing  -> "my:"


instance (IsResource res, IsAbility abl) => ToJSON (Capability res abl) where
  toJSON = \case
    CapResource res ability ->
      object
        [ "with" .= String (renderResource res)
        , "can"  .= String (textDisplay ability)
        ]

    CapOwnedResources owned ->
      toJSON owned

    CapProofRedelegation redel ->
      toJSON redel

data AsOrMyOrPrf
  = My (Maybe (URI.RText 'URI.Scheme))
  | As DID (Maybe (URI.RText 'URI.Scheme))
  | Prf (Maybe Natural)

parseScheme :: Parse.Parser (URI.RText 'URI.Scheme)
parseScheme = do
  x <- Parse.satisfy (\c -> Char.isAscii c || Char.isAlpha c)
  xs <- Parse.takeWhile (\c -> Char.isAscii c || Char.isAlphaNum c || c `elem` ("+-." :: String))
  case URI.mkScheme $ Text.singleton x <> xs of
    Right scheme   -> return scheme
    Left exception -> fail $ "Can't parse scheme: " <> show exception

parseStarOr :: Parse.Parser a -> Parse.Parser (Maybe a)
parseStarOr alternative =
  let parseStar = fmap (const Nothing) $ Parse.char '*'
  in fmap Just alternative <|> parseStar


data MAP = M | A | P

parseAsOrMyOrPrf :: Parse.Parser AsOrMyOrPrf
parseAsOrMyOrPrf = do
  prefix <- Parse.string "my:"  *> return M
        <|> Parse.string "as:"  *> return A
        <|> Parse.string "prf:" *> return P
  case prefix of
    P -> Prf <$> parseStarOr Parse.decimal
    M -> My  <$> parseStarOr parseScheme
    A -> As  <$> DID.parse <* Parse.string ":" <*> parseStarOr parseScheme


instance (IsResource res, IsAbility abl) => FromJSON (Capability res abl) where
  parseJSON = withObject "UCAN.Capability" \obj -> do
    withField <- obj .: "with"
    canField  <- obj .: "can"

    case (withField, canField) of
      (String with, String can) ->
        case Parse.parseOnly (parseAsOrMyOrPrf <* Parse.endOfInput) with of
          Right (My Nothing) -> do
            when (can /= "*") do
              fail "The 'my:*' resource must have the ability set to '*'"

            return $ CapOwnedResources $ OwnedResources Nothing All

          Right (As did Nothing) -> do
            when (can /= "*") do
              fail "They 'as:<did>:*' resource must have the ability set to '*'"

            return $ CapOwnedResources $ OwnedResources (Just did) All

          Right (My (Just scheme)) -> do
            ability <- parseJSON canField
            return $ CapOwnedResources $ OwnedResources Nothing (OnlyScheme scheme ability)

          Right (As did (Just scheme)) -> do
            ability <- parseJSON canField
            return $ CapOwnedResources $ OwnedResources (Just did) (OnlyScheme scheme ability)

          Right (Prf idx) -> do
            when (Text.toLower can /= "ucan/delegate") do
              fail "The 'prf' resource must have the ability set to 'ucan/DELEGATE'"

            return $ CapProofRedelegation $ maybe RedelegateAllProofs RedelegateProof idx

          Left _ -> do
            resource <- case parseResource with of
              JSON.Success res   -> return res
              JSON.Error message -> fail message
            ability <- parseJSON canField
            return $ CapResource resource ability

      _ ->
        fail "'with' and 'can' fields must be strings"
