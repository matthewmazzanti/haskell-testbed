{-# LANGUAGE MagicHash #-}

import GHC.ST
import GHC.Prim

get :: ST (STRep s a) -> ST (STRep s (State# s))
