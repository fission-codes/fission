module Fission.Platform.Heroku.Types
  ( ID (..)
  , Password (..)
  )where

newtype ID = ID { getID :: ByteString }
  deriving ( Show
           , Eq
           , IsString
           )

newtype Password = Password { getPassword :: ByteString }
  deriving ( Show
           , Eq
           , IsString
           )
