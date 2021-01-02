module Fission.Email.SendInBlue.Request.Types (Request(..)) where

import           Fission.Prelude                                hiding (to)

import           Fission.Email.Recipient.Types
import           Fission.Email.SendInBlue.TemplateId.Types
import           Fission.Email.SendInBlue.TemplateOptions.Types

import qualified Data.Vector                                    as Vector
import qualified RIO.NonEmpty                                   as NonEmpty


data Request = Request
  { templateId :: TemplateId
  , to         :: NonEmpty Recipient
  , params     :: TemplateOptions
  }

instance ToJSON Request where
  toJSON Request { templateId, to, params } =
    Object [ ("templateId", Number $ fromIntegral (unTemplateId templateId))
           , ("to",         Array . Vector.fromList $ NonEmpty.toList (toJSON <$> to))
           , ("params",     toJSON params)
           ]
