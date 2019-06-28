module Fission.IPFS.Error (Add (..)) where

import RIO

data Add = InvalidFile
         | UnexpectedOutput Text
         | UnknownError
         deriving ( Eq
                  , Show
                  )
