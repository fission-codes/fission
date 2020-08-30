module Fission.Email.Recipient.Types (Recipient(..)) where

import Fission.Prelude
import Fission.User.Email.Types
import Fission.User.Username.Types

data Recipient = Recipient 
  { email :: !Email
  , name  :: !Username
  }

instance ToJSON Recipient where
  toJSON Recipient { email, name } =
    Object [ ("email", toJSON email)
           , ("name",  toJSON name)
           ]
