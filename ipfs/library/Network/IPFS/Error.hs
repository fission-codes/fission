module Network.IPFS.Error
  ( Error (..)
  , Linearization (..)
  ) where

import           Network.IPFS.Prelude
import           Network.IPFS.Types

import qualified Network.IPFS.Add.Error as Add
import qualified Network.IPFS.Get.Error as Get

data Error
  = AddErr Add.Error
  | GetErr Get.Error
  | LinearizationErr Linearization
  deriving ( Exception
           , Eq
           , Generic
           , Show
           )

-- NOTE Will not stay as a newtype in the long term
newtype Linearization = NonLinear SparseTree
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( Exception
                    , ToJSON
                    )

instance Display Linearization where
  display (NonLinear sparseTree) = "Unable to linearize IPFS result: " <> display sparseTree
