module Fission.Email.SendInBlue.Response.Types (Response(..)) where

import Fission.Prelude


newtype Response = Response { messageId :: Text }
  deriving newtype  ( Eq
                    , Show
                    , IsString
                    , Display
                    )

instance FromJSON Response where
  parseJSON = withObject "Response" \obj -> do
    messageId <- obj .:  "messageId"

    return Response {..}

