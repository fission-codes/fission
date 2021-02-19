module Fission.Unit.Prefix.Class (FromPrefixed (..), ToPrefixed (..)) where

class FromPrefixed prefix n where
  fromPrefixed :: prefix n -> n

class ToPrefixed prefix n where
  toPrefixed :: n -> prefix n
