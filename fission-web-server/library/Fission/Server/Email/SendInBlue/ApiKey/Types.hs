module Fission.Email.SendInBlue.ApiKey.Types (ApiKey(..)) where

import Fission.Prelude

newtype ApiKey = ApiKey { unApiKey :: Text }
  deriving newtype  ( Eq
                    , Show
                    , IsString
                    )

instance FromJSON ApiKey where
  parseJSON = withText "ApiKey" (pure . ApiKey)

