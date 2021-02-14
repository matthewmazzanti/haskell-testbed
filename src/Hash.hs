{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Hashable

data Foo a
    = Foo a String
    deriving (Eq, Generic, Show)

instance Hashable a => Hashable (Foo a)

data Color
    = Red
    | Green
    | Blue
    deriving (Generic, Show)

instance Hashable Color

test1 = Foo Red "hello"
test2 = Foo Red "hello"
