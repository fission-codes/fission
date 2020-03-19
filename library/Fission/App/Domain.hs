module Fission.App.Domain
  ( module Fission.App.Domain.Error
  , module Fission.App.Domain.Types

  , module Fission.App.Domain.Initializer
  , module Fission.App.Domain.Associator
  , module Fission.App.Domain.Dissociator
  , module Fission.App.Domain.Retriever

  , Actions
  ) where

import Fission.Prelude

import Fission.App.Domain.Error
import Fission.App.Domain.Types

import Fission.App.Domain.Initializer
import Fission.App.Domain.Associator
import Fission.App.Domain.Dissociator (Dissociator (..))
import Fission.App.Domain.Retriever   (Retriever   (..))

type Actions m
  = ( Initializer m
    , Associator  m
    , Dissociator m
    , Retriever   m
    )

foo :: (Creator m, Retriever m) => m ()
foo =
  allSiblingsByDomain defaultDomain Nothing >>= \case
    Right _ ->
      return ()

    Left -> do

      return ()

  -- maybeDomain <- lookup domain name with default domain
  -- case maybeDomain of
  --   Just _ -> noop
  --   Nothing -> create domain
