module Fission.Internal.List (difference, without) where

import RIO.List ((\\))


{-| Get the difference between two lists.

    >>> difference [ 1, 2, 3 ] [ 2, 3 ]
    [ 1 ]

-}
difference =
  (\\)


{-| Get the difference between two lists,
    flipped version of `difference`.

    >>> without [ 2, 3 ] [ 1, 2, 3 ]
    [ 1 ]

-}
without =
  flip (\\)
