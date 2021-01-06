module Fission.Web.Server.Email.SendInBlue.TemplateId.Types (TemplateId(..)) where

import           Fission.Prelude

newtype TemplateId = TemplateId { unTemplateId :: Int64 }
  deriving newtype  ( Eq
                    , Show
                    , FromJSON
                    )
