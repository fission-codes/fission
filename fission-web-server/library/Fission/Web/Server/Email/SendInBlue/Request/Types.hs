module Fission.Web.Server.Email.SendInBlue.Request.Types (Request(..)) where

import qualified Data.Vector                                          as Vector
import qualified RIO.NonEmpty                                         as NonEmpty

import           Fission.Prelude                                      hiding
                                                                      (to)

import           Fission.Web.Server.Email.Recipient.Types
import           Fission.Web.Server.Email.SendInBlue.TemplateId.Types

data Request = Request
  { templateId :: TemplateId
  , to         :: NonEmpty Recipient
  , params     :: Value
  }

instance ToJSON Request where
  toJSON Request { templateId, to, params } =
    Object [ ("templateId", Number $ fromIntegral (unTemplateId templateId))
           , ("to",         Array . Vector.fromList $ NonEmpty.toList (toJSON <$> to))
           , ("params",     params)
           ]
